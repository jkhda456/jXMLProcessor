{
  jXMLProcessor is licensed under the
  GNU Lesser General Public License v3.0
}

unit AXMLParser;

interface

uses
  SysUtils, Classes, AXMLConst;

type
  TStringChunk = record
     ChunkType: Longword;
     ChunkSize: Longword;
     StringCount: Longword;
     StyleCount: Longword;
     Flags: Longword;
     StringPoolOffset: Longword;
     StylePoolOffset: Longword;
     StringOffsetArray: Array of Longword;
     StyleOffsetArray: Array of Longword;
  end;
  PStringChunk = ^TStringChunk;

  TResourceIdChunk = record
    ChunkType: Longword;
    ChunkSize: Longword;
  end;
  PResourceIdChunk = ^TResourceIdChunk;

  TAttributeItem = record
    Namespace: Longword;
    Name: Longword;
    RawValue: Longword;

    Size: Word;
    Res0: Byte;
    DataType: Byte;
    Data: Longword;
  end;
  PAttributeItem = ^TAttributeItem;

  TAbstractChunk = record
    ChunkType: Word;
    
    ChunkHeaderSize: Word;
    ChunkSize: Longword;

    ChunkLevel: Integer;

    ChunkStartPos,
    ChunkEndPos: Longword; // for virtual
  end;
  PAbstractChunk = ^TAbstractChunk;

  TNamespaceStack = record
    Prefix: Longword;
    Uri: Longword;
    StartIdx,
    EndIdx: Integer;
  end;
  PNamespaceStack = ^TNamespaceStack;

  // attributes count does not exist (for reliability)
  TReadFlag = (rfXMLType, rfTagNameOnly, rfTagNamesLevel, rfTagLevel, rfTagAttributes, rfTagAttributeStringList, rfAnalysis);
  TReplaceFlag = (repfTagName, repfPropertyName, repfPrepertyValue);
  TMakeFlag = (mkfTag, mkfProperty);
  TTagOutputFormat = (tofDefault, tofStringList);

  TAXMLParser = class
  private
    FAXMLFileStream: TStream;

    FHeader: Longword;
    FFullSize: Longword;
    FNamespaceStack: TList;
    FChunkList: TList;

    FCursorFileOffset: Longword;
    FPrintInitParsePos: Boolean;

    // Utils
    function PopBYTEDataFromChunk: Byte;
    function PopWORDDataFromChunk: Word;
    function PopDWORDDataFromChunk: Longword;
    function PopNullTerminateStr: String;
    function PopSizeAssignedStr(ASize: Longword): String;
    function PopSizeAssignedWideStr(ASize: Longword): WideString;
    function PopUserData(var OutData; DataSize: Longword): Integer;

    function IncDWORDData(AIncSize: Integer): Longword;
    function IncWORDData(AIncSize: Integer): Word;

    procedure ReleaseStream;
    procedure ReleaseLists;

    procedure TestParser;
  protected
    function LoadAXMLStream: Integer;

    // Position unsafe methods
    function GetResourceId(const ResIdx: Integer): Integer;
    function GetResourceString(Id: Longword): String;
    function GetResourceIndex(const ResourceStr: String): Integer;
    function GetStringPool(const Index: Longword; Trimming: Boolean = True): String;
    function SearchStringPool(TargetString: String): Integer;
    function CountStringPool: Integer;

    // Cursor based function
    procedure ResetCursor;
    function SearchTagInDepth(SearchTag: String): Integer;
    function CountTagInDepth: Integer;
    function SearchNamespacePrefix(SearchPrefix: String): Longword;
    function GetNamespacePrefix(NamespaceUriIndex: Longword): String;
    function GetTag(const ListIndex: Integer; TagType: TTagOutputFormat = tofDefault; ProcessNamespace: Boolean = True): String;
    function GetStream(const ListIndex: Integer): String;
  public
    constructor Create;
    destructor Destroy;

    function ReloadAXMLStream: Integer;
    function IsValidAXML: Integer;  // Not work..

    function AppendStringPool(NewString: String): Integer;

    function TagsCount: Integer;
    // rfXMLType                : AFlag - tab control. (1 : on)
    // rfTagAttributeStringList : AFlag - namespace control. (0: off, 1: on)
    function ReadTags(AIndex: Integer; AReadFlag: TReadFlag; AFlag: Integer = 0): String;
    // AValueType must be $03!!!
    function RepleaceTags(AIndex: Integer; AReplaceFlag: TReplaceFlag; AValueType: Integer; ASearchValue, AReplaceValue: String): Integer;
    function MakeTags(AIndex: Integer; AMakeFlag: TMakeFlag; AValueType: Integer; AName, AValue: String): Integer;

    function SaveAsFile(AFileName: TFileName): Integer;
    function LoadFromFile(AFileName: TFileName; AOnMemory: Boolean = True): Integer;

    property PrintInitParsePos: Boolean read FPrintInitParsePos write FPrintInitParsePos;
  end;

implementation

const
  CONST_StringResourceLimit = $1000;

  CONST_AXMLHeader = $00080003;

  CONST_RType_NULL = $0000;
  CONST_RType_StringPool = $0001;
  CONST_RType_Table = $0002;
  CONST_RType_XML = $0003;

  CONST_RType_XMLFirstChunk = $0100;
  CONST_RType_XMLStartNamespace = $0100;
  CONST_RType_XMLEndNamespace = $0101;
  CONST_RType_XMLStartElement = $0102;
  CONST_RType_XMLEndElement = $0103;
  CONST_RType_XMLCDATA = $0104;
  CONST_RType_XMLLastChunk = $017F;
  CONST_RType_XMLResourceMap = $0180;

  CONST_RType_TablePackage = $0200;
  CONST_RType_TableType = $0201;
  CONST_RType_TableTypeSpec = $0202;
  CONST_RType_TableLibrary = $0203;

  CONST_BoolToStr: Array[Boolean] of string = ('false', 'true');

// copy code lol
procedure InsertStringToMemoryStream(MS: TMemoryStream;
  Index: Integer; const S: string);
var
  SLength, OldSize: Integer;
  Src, Dst, PointerToS: ^Char;
begin
  if Index > MS.Size then Exit; // Index := MS.Size;

  SLength := Length(S);
  OldSize := MS.Size;
  MS.SetSize(MS.Size + SLength);
  Src := MS.Memory; Inc(Src, Index);
  Dst := Src; Inc(Dst, SLength);
  Move(Src^, Dst^, OldSize - Index);
  PointerToS := @S[1];
  Move(PointerToS^, Src^, SLength);
end;

function TAXMLParser.AppendStringPool(NewString: String): Integer;
var
  StringPoolMax: Integer;
  LoopVar: Integer;
  WorkChunk: PAbstractChunk;
  KeepPosition,
  NewPoolOffset,
  OldValue,
  NewValue: Longword;
  UTF8Flag: Boolean;
  WorkStrChunk: TStringChunk;
  StrPoolReadLoop: Integer;
  OffsetStart,
  LastOffset: Longword;
  ReadStrLength: Longword;
  WriteStringLength: Longword;
  WorkWord: Word;
  ModByte: Byte;
  WriteStringLengthByte, WorkByte: Byte;
  WriteString: String;
  NewStringW: WideString;

  function IncToChunk(IncSize: Integer): Longword;
  begin
    KeepPosition := FAXMLFileStream.Position;
    OldValue := PopDWORDDataFromChunk;
    Result := OldValue;
    NewValue := OldValue + IncSize;
    FAXMLFileStream.Position := KeepPosition;
    FAXMLFileStream.Write(NewValue, SizeOf(Longword));
  end;
begin
  StringPoolMax := CountStringPool;

  // need append new item.
  For LoopVar := FChunkList.Count-1 DownTo 0 do
  Begin
     WorkChunk := FChunkList[LoopVar];
     If WorkChunk^.ChunkType <> CONST_RType_StringPool Then Continue;
     // Modify last item only!

     With WorkChunk^ do
     Begin
        FAXMLFileStream.Position := ChunkStartPos + 4;

        PopDWORDDataFromChunk; // ChunkSize
        // Change String Count;
        WorkStrChunk.StringCount := IncToChunk(1);

        // Read step.
        WorkStrChunk.StyleCount := PopDWORDDataFromChunk; // Style Count
        WorkStrChunk.Flags := PopDWORDDataFromChunk; // Flags
        UTF8Flag := (WorkStrChunk.Flags and 256) <> 0; // (1 << 8)

        OffsetStart := FAXMLFileStream.Position;
        WorkStrChunk.StringPoolOffset := PopDWORDDataFromChunk; // Useless..
        WorkStrChunk.StylePoolOffset := PopDWORDDataFromChunk; // Useless..

        // getting string last pool info
        FAXMLFileStream.Position := FAXMLFileStream.Position + ((WorkStrChunk.StringCount-1) * Sizeof(Longword));
        LastOffset := PopDWORDDataFromChunk;
        KeepPosition := FAXMLFileStream.Position;

        FAXMLFileStream.Position := WorkStrChunk.StringPoolOffset + LastOffset + 8;
        If UTF8Flag Then
        Begin
           //
           PopBYTEDataFromChunk;
           ReadStrLength := PopBYTEDataFromChunk;

           If (ReadStrLength and $80) <> 0 Then
           Begin
              PopBYTEDataFromChunk;
           End;

           If (ReadStrLength <> 0) Then
              PopNullTerminateStr();

           WriteString := #0#0 + NewString + #0;
           WorkByte := Length(NewString);

           // align check!
           ModByte := (Length(WriteString) And $3);
           If ModByte <> 0 Then
           Begin
              WriteString := WriteString + StringOfChar(#0, ModByte);
              WriteStringLengthByte := Length(WriteString);
           End;

           Move(WorkByte, WriteString[2], 1);
           // Insert New String
           InsertStringToMemoryStream(FAXMLFileStream as TMemoryStream, FAXMLFileStream.Position, WriteString);

           NewPoolOffset := LastOffset + ReadStrLength + 3;
           SetLength(WriteString, SizeOf(NewPoolOffset));
           Move(NewPoolOffset, WriteString[1], SizeOf(NewPoolOffset));
           // Insert PoolOffset
           InsertStringToMemoryStream(FAXMLFileStream as TMemoryStream, KeepPosition, WriteString);

           FAXMLFileStream.Position := OffsetStart;
           IncToChunk(4); // String Pool offset
           IncToChunk(Length(NewString) + 3); // Style Pool offset

           FAXMLFileStream.Position := ChunkStartPos + 4;
           // Change ChunkSize (+4)
           IncToChunk(4 + WriteStringLengthByte);
        End
        Else
        Begin
           ReadStrLength := PopWORDDataFromChunk;

           If (ReadStrLength and $8000) <> 0 Then
           Begin
              ReadStrLength := ((ReadStrLength and $7FFF) shl 16) or PopWORDDataFromChunk;
           End;

           NewStringW := PopSizeAssignedWideStr(ReadStrLength);

           PopWORDDataFromChunk; // jump last null space.

           // Convert Wide string.
           NewStringW := NewString;
           SetLength(WriteString, Length(NewString) * SizeOf(WideChar));
           Move(NewStringW[1], WriteString[1], Length(NewString) * SizeOf(WideChar));
           WriteString := #0#0 + WriteString + #0#0;
           WriteStringLength := Length(WriteString);

           // align check!
           ModByte := (Length(WriteString) And $3);
           If ModByte <> 0 Then
           Begin
              WriteString := WriteString + StringOfChar(#0, ModByte);
              WriteStringLength := Length(WriteString);
           End;

           WorkWord := Length(NewStringW);
           Move(WorkWord, WriteString[1], 2);
           // Insert New String
           InsertStringToMemoryStream(FAXMLFileStream as TMemoryStream, FAXMLFileStream.Position, WriteString);

           NewPoolOffset := LastOffset + (ReadStrLength * SizeOf(WideChar)) + 4;
           SetLength(WriteString, SizeOf(NewPoolOffset));
           Move(NewPoolOffset, WriteString[1], SizeOf(NewPoolOffset));
           // Insert PoolOffset = Last + NewString Full Length.
           InsertStringToMemoryStream(FAXMLFileStream as TMemoryStream, KeepPosition, WriteString);

           FAXMLFileStream.Position := OffsetStart;
           IncToChunk(4); // String Pool offset
           IncToChunk(WriteStringLength); // Style Pool offset

           FAXMLFileStream.Position := ChunkStartPos + 4;
           // Change ChunkSize (+4)
           IncToChunk(4 + WriteStringLength);
        End;

     End;

     Result := StringPoolMax;

     Exit;
  End;
end;

function TAXMLParser.CountStringPool: Integer;
var
  ChunkLoop: Integer;
  LoopVar: Integer;
  WorkChunk: PAbstractChunk;
  WorkStrChunk: TStringChunk;

  StrPoolReadLoop: Integer;
  StrPoolList: TList;

  UTF8Flag: Boolean;
  ReadStrLength: Longword;
  LastOffset: Longword;
begin
  Result := 0;
  LoopVar := 0;
  If Not Assigned(FAXMLFileStream) Then Exit;

  // Init Structure...
  FillChar(WorkStrChunk, SizeOf(WorkStrChunk), 0);

  StrPoolList := TList.Create;
  Try
     For ChunkLoop := 0 to FChunkList.Count-1 do
     Begin
        WorkChunk := FChunkList[ChunkLoop];
        If WorkChunk^.ChunkType <> CONST_RType_StringPool Then Continue;

        // ChunkFound.
        With WorkChunk^ do
        Begin
           // do not trust String Count.
           // Type | Size | Str Count | Style Count | Reserved | Str Pool Offset | Style Pool Offset | Str Offsets | Style Offsets

           // Not Valid Chunk! Step NEXT.
           If ChunkEndPos <= 0 Then Continue;

           WorkStrChunk.ChunkType := ChunkType;
           FAXMLFileStream.Position := ChunkStartPos + 4;

           WorkStrChunk.ChunkSize := PopDWORDDataFromChunk;
           WorkStrChunk.StringCount := PopDWORDDataFromChunk;
           WorkStrChunk.StyleCount := PopDWORDDataFromChunk; // Useless..
           WorkStrChunk.Flags := PopDWORDDataFromChunk; // Useless..
           UTF8Flag := (WorkStrChunk.Flags and 256) <> 0; // (1 << 8)

           WorkStrChunk.StringPoolOffset := PopDWORDDataFromChunk; // Useless..
           WorkStrChunk.StylePoolOffset := PopDWORDDataFromChunk; // Useless..

           // Check Data for Obfuscate...
           If (WorkStrChunk.StringPoolOffset >= ChunkEndPos) or (WorkStrChunk.StringPoolOffset <= ChunkStartPos) Then
              WorkStrChunk.StringPoolOffset := 0; // do not use

           Result := Result + WorkStrChunk.StringCount;
        End;
     End;

  Finally
     StrPoolList.Free;
  End;
end;

function TAXMLParser.CountTagInDepth: Integer;
begin
  
end;

constructor TAXMLParser.Create;
begin
  inherited;

  FPrintInitParsePos := False;
  FAXMLFileStream := Nil;

  FChunkList := TList.Create;
  FNamespaceStack := TList.Create;
end;

destructor TAXMLParser.Destroy;
begin
  ReleaseLists;
  FChunkList.Free;
  FNamespaceStack.Free;

  ReleaseStream;

  inherited;
end;

function TAXMLParser.GetNamespacePrefix(
  NamespaceUriIndex: Longword): String;
var
  LoopVar: Integer;
begin
  For LoopVar := 0 to FNamespaceStack.Count-1 do
  Begin
     With PNamespaceStack(FNamespaceStack[LoopVar])^ do
     Begin
        If Uri = NamespaceUriIndex Then
        Begin
           Result := GetStringPool(Prefix);
           Exit;
        End;
     End;
  End;
end;

function TAXMLParser.GetResourceId(const ResIdx: Integer): Integer;
const
  ResourceChunkBaseSize = 8;
var
  ChunkLoop: Integer;
  IndexVar: Integer;
  LoopVar: Integer;
  LoopMax: Integer;
  IdCandidate: Integer;
  WorkChunk: PAbstractChunk;
begin
  Result := 0;
  LoopVar := 0;
  IndexVar := 0;
  If Not Assigned(FAXMLFileStream) Then Exit;

  For ChunkLoop := 0 to FChunkList.Count-1 do
  Begin
     WorkChunk := FChunkList[ChunkLoop];
     If WorkChunk^.ChunkType <> CONST_RType_XMLResourceMap Then Continue;

     FAXMLFileStream.Position := WorkChunk^.ChunkStartPos + 8;
     LoopMax := (WorkChunk^.ChunkSize - ResourceChunkBaseSize) div 4;
     For LoopVar := 0 to LoopMax do
     Begin
        IdCandidate := PopDWORDDataFromChunk;

        If (IndexVar >= ResIdx) Then
        Begin
           Result := (IdCandidate - $1010000);
           Exit;
        End;
        IndexVar := IndexVar + 1;
     End;
  End;
end;

function TAXMLParser.GetResourceIndex(const ResourceStr: String): Integer;
var
  LoopVar: Integer;
begin
  Result := -1;
  For LoopVar := Low(CONST_ResourceStr) to High(CONST_ResourceStr) do
  Begin
     If CONST_ResourceStr[LoopVar] = ResourceStr Then
     Begin
        Result := LoopVar;
        Exit;
     End;
  End; 
end;

function TAXMLParser.GetResourceString(Id: Longword): String;
var
  ParseId: Longword;
begin
  Result := '';
  ParseId := GetResourceId(Id);
  If ParseId <= High(CONST_ResourceStr) Then
  Begin
     Result := CONST_ResourceStr[ParseId];
  End;
end;

function TAXMLParser.GetStream(const ListIndex: Integer): String;
var
  WorkChunk: PAbstractChunk;
  ChunkCalcSize: Integer;
  ReadBuffer: String;

  function StringToHex(const S: string): string;
  var
    LoopVar: Integer;
  begin
    Result := '';
    For LoopVar := 1 to Length(S) do
    Begin
       Result := Result + IntToHex( Byte( S[LoopVar] ), 2 ) + ' ';
       If (LoopVar Mod 16) = 0 Then
          Result := Result + #13#10;
    End;
  end;

begin
  Result := '';
  If (ListIndex < 0) or (ListIndex >= FChunkList.Count) Then Exit;
  If Not Assigned(FAXMLFileStream) Then Exit;

  WorkChunk := FChunkList[ListIndex];

  With WorkChunk^ do
  Begin
     // SetLength(ReadBuffer,
     FAXMLFileStream.Position := ChunkStartPos;
     ChunkCalcSize := ChunkEndPos - ChunkStartPos;
     If ChunkCalcSize <= 0 Then Exit;

     SetLength(ReadBuffer, ChunkCalcSize);
     FAXMLFileStream.Read(ReadBuffer[1], ChunkCalcSize);

     Result := StringToHex(ReadBuffer);
  End;
end;

function TAXMLParser.GetStringPool(const Index: Longword; Trimming: Boolean): String;
var
  ChunkLoop: Integer;
  LoopVar: Integer;
  WorkChunk: PAbstractChunk;
  WorkStrChunk: TStringChunk;

  StrPoolReadLoop: Integer;
  StrPoolList: TList;

  UTF8Flag: Boolean;
  ReadStrLength: Longword;
  LastOffset: Longword;
begin
  Result := '';
  LoopVar := 0;
  If Not Assigned(FAXMLFileStream) Then Exit;

  // Init Structure...
  FillChar(WorkStrChunk, SizeOf(WorkStrChunk), 0);

  StrPoolList := TList.Create;
  Try
     For ChunkLoop := 0 to FChunkList.Count-1 do
     Begin
        WorkChunk := FChunkList[ChunkLoop];
        If WorkChunk^.ChunkType <> CONST_RType_StringPool Then Continue;

        // ChunkFound.
        With WorkChunk^ do
        Begin
           // do not trust String Count.
           // Type | Size | Str Count | Style Count | Reserved | Str Pool Offset | Style Pool Offset | Str Offsets | Style Offsets

           // Not Valid Chunk! Step NEXT.
           If ChunkEndPos <= 0 Then Continue;

           WorkStrChunk.ChunkType := ChunkType;
           FAXMLFileStream.Position := ChunkStartPos + 4;

           WorkStrChunk.ChunkSize := PopDWORDDataFromChunk;
           WorkStrChunk.StringCount := PopDWORDDataFromChunk;
           WorkStrChunk.StyleCount := PopDWORDDataFromChunk; // Useless..
           WorkStrChunk.Flags := PopDWORDDataFromChunk; // Useless..
           UTF8Flag := (WorkStrChunk.Flags and 256) <> 0; // (1 << 8)

           WorkStrChunk.StringPoolOffset := PopDWORDDataFromChunk; // Useless..
           WorkStrChunk.StylePoolOffset := PopDWORDDataFromChunk; // Useless..

           // StartCandidateOffset

           // Check Data for Obfuscate...
           If (WorkStrChunk.StringPoolOffset >= ChunkEndPos) or (WorkStrChunk.StringPoolOffset <= ChunkStartPos) Then
              WorkStrChunk.StringPoolOffset := 0; // do not use

           For StrPoolReadLoop := WorkStrChunk.StringCount DownTo 0 do
           Begin
              If (FAXMLFileStream.Position >= ChunkEndPos) Then Break; // Bad Chunk.

              LastOffset := PopDWORDDataFromChunk;
              StrPoolList.Add( Pointer(LastOffset) );

              // Found!
              If StrPoolList.Count >= Index+1 Then
              Begin
                 FAXMLFileStream.Position := WorkStrChunk.StringPoolOffset + LastOffset + 8;

                 If UTF8Flag Then
                 Begin
                    //
                    PopBYTEDataFromChunk;
                    ReadStrLength := PopBYTEDataFromChunk;

                    If (ReadStrLength and $80) <> 0 Then
                    Begin
                       PopBYTEDataFromChunk;
                    End;

                    If (ReadStrLength <> 0) Then
                    Begin
                       Result := PopNullTerminateStr();
                    End;
                 End
                 Else
                 Begin
                    ReadStrLength := PopWORDDataFromChunk;

                    If (ReadStrLength and $8000) <> 0 Then
                    Begin
                       ReadStrLength := ((ReadStrLength and $7FFF) shl 16) or PopWORDDataFromChunk;
                    End;

                    Result := PopSizeAssignedWideStr(ReadStrLength);
                 End;

                 If Trimming Then
                    Result := Trim(Result);

                 If Trim(Result) = '' Then
                 Begin
                    Result := GetResourceString(Index);
                 End;

                 Exit;
              End;
           End;
        End;
     End;

  Finally
     StrPoolList.Free;
  End;
end;

function TAXMLParser.GetTag(const ListIndex: Integer; TagType: TTagOutputFormat; ProcessNamespace: Boolean): String;
var
  WorkChunk: PAbstractChunk;
  NameSpace,
  TagName: Longword;
  AttributeCount: Longword;
  LoopVar: Integer;
  AttributeList: TList;
  AttributeData: PAttributeItem;
  TagValueStr: String;

  function AttributeToStr(const InputAttribute: TAttributeItem): String;
  var
    AtrributeNamespaceUri: Longword;
    AttributeValue: String;
  begin
    AtrributeNamespaceUri := AttributeData^.Namespace;
    Result := GetStringPool(AttributeData^.Name);
    If (AtrributeNamespaceUri <> $FFFFFFFF) and ProcessNamespace Then
    Begin
       Result := GetNamespacePrefix(AtrributeNamespaceUri) + ':' + Result;
    End;

    Case InputAttribute.DataType of
       $00: Exit; // null
       $01: AttributeValue := '@' + IntToStr(AttributeData^.Data); // reference
       //$02: // attribute
       $03: AttributeValue := GetStringPool(AttributeData^.Data); // string
       //$04: // float
       //$05: // dimension
       //$06: // fraction
       //$07: // reference

       // $10: // frist int
       //$10: // int dec
       $11: AttributeValue := '@' + IntToHex(AttributeData^.Data, 8); // int hex
       $12: AttributeValue := CONST_BoolToStr[(AttributeData^.Data and 1) = 1]; // int bool

       // $1C: // first color
       //$1C: // #AArrggbb
       //$1D: // #rrggbb
       //$1E: // #argb
       //$1F: // #rgb
       // $1F: // last color

       // $1F: // last int
    Else
       AttributeValue := GetStringPool(AttributeData^.Data);
       // Result := Result + '="' + IntToStr(AttributeData^.Data) + '"'
    End;

    Case TagType of
       tofDefault: Result := Result + '="' + AttributeValue + '"';
       tofStringList: Result := Result + '=' + AttributeValue;
    End;
  end;
begin
  // (!) Name Space not care..

  Result := '';
  If (ListIndex < 0) or (ListIndex >= FChunkList.Count) Then Exit;

  WorkChunk := FChunkList[ListIndex];

  With WorkChunk^ do
  Begin
     If (ChunkType <> CONST_RType_XMLStartElement) and (ChunkType <> CONST_RType_XMLEndElement) Then Exit;

     FAXMLFileStream.Position := ChunkStartPos + 8;

     PopDWORDDataFromChunk; // line number
     PopDWORDDataFromChunk; // comment
     NameSpace := PopDWORDDataFromChunk; // namespace
     TagName := PopDWORDDataFromChunk; // name

     Case ChunkType of
        CONST_RType_XMLStartElement:
        Begin
           PopDWORDDataFromChunk; // attribute size
           AttributeCount := PopWORDDataFromChunk; // attribute count
           PopWORDDataFromChunk; // id index
           PopWORDDataFromChunk; // class index
           PopWORDDataFromChunk; // style index

           AttributeList := TList.Create;
           Try
              For LoopVar := 1 to AttributeCount do
              Begin
                 GetMem(AttributeData, SizeOf(TAttributeItem));
                 PopUserData(AttributeData^, SizeOf(TAttributeItem));

                 AttributeList.Add( AttributeData );
              End;

              TagValueStr := GetStringPool(TagName);
              Result := TagValueStr;
              
              For LoopVar := 0 to AttributeList.Count-1 do
              Begin
                 AttributeData := AttributeList[LoopVar];

                 // dup PushValueToResult.
                 Case TagType of
                    tofDefault: Result := Result + ' ' + AttributeToStr(AttributeData^);
                    tofStringList: Result := Result + #13#10 + AttributeToStr(AttributeData^);
                 End;

                 FreeMem(AttributeData);
              End;
           Finally
              AttributeList.Free;
           End;

           // manifest have namespace assign
           If (LowerCase(TagValueStr) = 'manifest') and (TagType = tofDefault) Then
           Begin
              For LoopVar := 0 to FNamespaceStack.Count-1 do
              Begin
                 With PNamespaceStack(FNamespaceStack[LoopVar])^ do
                 Begin
                    Result := Result + #13#10 + 'xmlns:' + GetStringPool(Prefix) + '="'+GetStringPool(Uri)+'"'
                 End;
              End;
           End;
        End;
        CONST_RType_XMLEndElement:
        Begin
           If TagType = tofDefault Then
              Result := '/' + GetStringPool(TagName);
        End;
     End;

     If TagType = tofDefault Then
        Result := '<' + Result + '>';
  End;
end;

function TAXMLParser.IncDWORDData(AIncSize: Integer): Longword;
var
  OldValue, NewValue,
  KeepPosition: Longword;
begin
  KeepPosition := FAXMLFileStream.Position;
  OldValue := PopDWORDDataFromChunk;
  Result := OldValue;
  If OldValue <= 0 Then Exit;

  NewValue := OldValue + AIncSize;
  FAXMLFileStream.Position := KeepPosition;
  FAXMLFileStream.Write(NewValue, SizeOf(Longword));
end;

function TAXMLParser.IncWORDData(AIncSize: Integer): Word;
var
  OldValue, NewValue: Word;
  KeepPosition: Longword;
begin
  KeepPosition := FAXMLFileStream.Position;
  OldValue := PopWORDDataFromChunk;
  Result := OldValue;
  If OldValue <= 0 Then Exit;

  NewValue := OldValue + AIncSize;
  FAXMLFileStream.Position := KeepPosition;
  FAXMLFileStream.Write(NewValue, SizeOf(Word));
end;

function TAXMLParser.IsValidAXML: Integer;
begin
  Result := -1; // not impl...
end;

function TAXMLParser.LoadAXMLStream: Integer;
var
  LoopVar: Integer;
  ReadSize: Integer;
  ParseChunk: Word;

  TagLevel,
  NameSpaceLevel: Integer;

  procedure ReadChunk;
  const
    ChunkHeaderSize = 8;
  var
    NewChunk: PAbstractChunk;
    NewNamespace: PNamespaceStack;
  begin
    GetMem(NewChunk, SizeOf(TAbstractChunk));
    FChunkList.Add(NewChunk);

    NewChunk^.ChunkType := ParseChunk;
    FAXMLFileStream.Read(NewChunk^.ChunkHeaderSize, 2);

    // Init Chunk Pos...
    NewChunk^.ChunkStartPos := 0;
    NewChunk^.ChunkEndPos := 0;
    NewChunk^.ChunkLevel := 0;

    If NewChunk^.ChunkHeaderSize >= 4 Then
    Begin
       // Next is ChunkSize
       FAXMLFileStream.Read(NewChunk^.ChunkSize, 4);

       NewChunk^.ChunkStartPos := FAXMLFileStream.Position - ChunkHeaderSize;
       NewChunk^.ChunkEndPos := FAXMLFileStream.Position + NewChunk^.ChunkSize - ChunkHeaderSize;

       FAXMLFileStream.Position := NewChunk^.ChunkEndPos;
    End;

    Case ParseChunk of
       CONST_RType_XMLStartNamespace:
       Begin
          //
          GetMem(NewNamespace, SizeOf(TNamespaceStack));
          FNamespaceStack.Add(NewNamespace);

          FAXMLFileStream.Position := NewChunk^.ChunkStartPos + 8;
          PopDWORDDataFromChunk; // line number
          PopDWORDDataFromChunk; // comment
          NewNamespace^.Prefix := PopDWORDDataFromChunk;
          NewNamespace^.Uri := PopDWORDDataFromChunk;
          NewNamespace^.StartIdx := FChunkList.Count-1;

          // for debug.
          // writeln(IntToStr(Integer(NewNamespace)));
          FAXMLFileStream.Position := NewChunk^.ChunkEndPos;
       End;

       CONST_RType_XMLEndNamespace:
       Begin
       End;
       
       CONST_RType_XMLStartElement:
       Begin
          TagLevel := TagLevel + 1;
          NewChunk^.ChunkLevel := TagLevel;
       End;
       CONST_RType_XMLEndElement:
       Begin
          NewChunk^.ChunkLevel := TagLevel;
          If TagLevel >= 0 Then
             TagLevel := TagLevel - 1;
       End;
    End;

  end;

begin
  Result := -10;
  ReleaseLists;
  If Not Assigned(FAXMLFileStream) Then Exit;

  // Check Size
  Result := -11;
  If FAXMLFileStream.Size < 8 Then Exit;

  FCursorFileOffset := 0;
  FAXMLFileStream.Position := 0;

  TagLevel := 0;
  NameSpaceLevel := 0;

  // Check Magic Number
  FAXMLFileStream.Read(FHeader, 4);
  Result := -100;
  If FHeader <> CONST_AXMLHeader Then Exit;

  // Prepare Safe Size Position
  FAXMLFileStream.Read(FFullSize, 4);

  If FPrintInitParsePos Then
     WriteLn('parser init...');

  // ReadChunk
  While True do
  Begin
     ReadSize := FAXMLFileStream.Read(ParseChunk, 2);
     // if not exists..
     If (ReadSize < 2) or (FFullSize <= FAXMLFileStream.Position) Then Break;

     If FPrintInitParsePos Then
        WriteLn(IntToStr(FAXMLFileStream.Position) + '(0x'+IntToHex(FAXMLFileStream.Position, 8)+') : ' + IntToStr(ParseChunk));

     Case ParseChunk of
        // Null Chunk is Exit!
        CONST_RType_NULL:
        Break;

        CONST_RType_StringPool,
        CONST_RType_Table,
        CONST_RType_XML,

        CONST_RType_XMLFirstChunk, // CONST_RType_XMLStartNamespace, // Dupplicate.
        CONST_RType_XMLEndNamespace,
        CONST_RType_XMLStartElement,
        CONST_RType_XMLEndElement,
        CONST_RType_XMLCDATA,
        CONST_RType_XMLLastChunk,
        CONST_RType_XMLResourceMap,

        CONST_RType_TablePackage,
        CONST_RType_TableType,
        CONST_RType_TableTypeSpec,
        CONST_RType_TableLibrary
        
        // Support Chunk?
        :ReadChunk;
     End;
  End;

  Result := 0;
end;

function TAXMLParser.LoadFromFile(AFileName: TFileName; AOnMemory: Boolean): Integer;
begin
  Result := -1;
  ReleaseStream;

  Result := -2;
  If Not FileExists(AFileName) Then Exit;

  If AOnMemory Then
  Begin
     FAXMLFileStream := TMemoryStream.Create;
     (FAXMLFileStream as TMemoryStream).LoadFromFile(AFileName);
  End
  Else
  Begin
     // no more support FileStream.
     // FAXMLFileStream := TFileStream.Create(AFileName, fmOpenReadWrite);
     Exit;
  End;

  Result := LoadAXMLStream;

  // for Debug!
  // TestParser;
end;

function TAXMLParser.MakeTags(AIndex: Integer; AMakeFlag: TMakeFlag;
  AValueType: Integer; AName, AValue: String): Integer;
var
  WorkChunk: PAbstractChunk;
  TagName: Longword;
  AttributeCount: Longword;
  LoopVar: Integer;
  AttributeList: TList;
  AttributeData: PAttributeItem;
  StringIndex: Integer;
  MaxValueIndex: Longword;
  NewName, NewValue, NewNameResIndex, InsertAttrIndex: Integer;
  NewAttribute: TAttributeItem;
  KeepPosition: Longword;
  PrevAttributeIndex: Integer;
  PrevAttributeName: String;
  WriteString: String;
  ParsedNamespace: String;
begin
  Result := -1;
  If (AIndex < 0) or (AIndex >= FChunkList.Count) Then Exit;

  WorkChunk := FChunkList[AIndex];
  If (WorkChunk^.ChunkType <> CONST_RType_XMLStartElement) Then Exit; // and (WorkChunk^.ChunkType <> CONST_RType_XMLEndElement) 

  Case AMakeFlag of
     mkfProperty:
     With WorkChunk^ do
     Begin
        FAXMLFileStream.Position := ChunkStartPos + 4;
        IncDWORDData( SizeOf(NewAttribute) ); // apply new Chunk size.

        PopDWORDDataFromChunk; // line number
        PopDWORDDataFromChunk; // comment
        PopDWORDDataFromChunk; // namespace
        TagName := PopDWORDDataFromChunk; // name

        Case ChunkType of
           CONST_RType_XMLStartElement:
           Begin
              FillChar(NewAttribute, SizeOf(NewAttribute), 0);

              PopWORDDataFromChunk; // attributeStart
              PopWORDDataFromChunk; // attributeSize (structure size)

              AttributeCount := IncWORDData(1); // attribute count
              PopWORDDataFromChunk; // id index
              PopWORDDataFromChunk; // class index
              PopWORDDataFromChunk; // style index

              KeepPosition := FAXMLFileStream.Position; // 2350 first.

              // Make New Attribute
              Begin
                 NewAttribute.Namespace := $FFFFFFFF; // assign namespace uri
                 StringIndex := Pos(':', AName);
                 If StringIndex > 0 then // has namespace.
                 Begin
                    ParsedNamespace := Copy(AName, 1, StringIndex-1);
                    NewAttribute.Namespace := SearchNamespacePrefix(ParsedNamespace);
                    If (NewAttribute.Namespace <> $FFFFFFFF) Then
                    Begin
                       Delete(AName, 1, StringIndex);
                    End;
                 End;
                 // For Add index.
                 NewNameResIndex := GetResourceIndex(AName);

                 // Make prepare!
                 NewName := SearchStringPool(AName);
                 NewValue := SearchStringPool(AValue);
                 MaxValueIndex := CountStringPool;

                 // set Name
                 If NewName < 0 Then
                 Begin
                    NewAttribute.Name := MaxValueIndex;
                    MaxValueIndex := MaxValueIndex + 1;
                 End
                 Else
                    NewAttribute.Name := NewName;
                 // set Value
                 If NewValue < 0 Then
                    NewAttribute.RawValue := MaxValueIndex
                 Else
                    NewAttribute.RawValue := NewValue;
                 NewAttribute.Size := $08; // Name Value pair.
                 NewAttribute.DataType := $03;
                 NewAttribute.Data := NewAttribute.RawValue;
              End;

              // set defaultInsertIndex
              InsertAttrIndex := -1;
              AttributeList := TList.Create;
              Try
                 FAXMLFileStream.Position := KeepPosition;
                 For LoopVar := 1 to AttributeCount do
                 Begin
                    GetMem(AttributeData, SizeOf(TAttributeItem));
                    PopUserData(AttributeData^, SizeOf(TAttributeItem));

                    AttributeList.Add( AttributeData );
                 End;

                 For LoopVar := 0 to AttributeList.Count-1 do
                 Begin
                    AttributeData := AttributeList[LoopVar];
                    If (InsertAttrIndex < 0) Then
                    Begin
                       PrevAttributeName := GetStringPool(AttributeData^.Name);
                       PrevAttributeIndex := GetResourceIndex(PrevAttributeName);
                       If (PrevAttributeIndex < 0) or (PrevAttributeIndex > NewNameResIndex) Then
                       Begin
                          InsertAttrIndex := LoopVar;
                       End;
                    End;

                    FreeMem(AttributeData);
                 End;
              Finally
                 AttributeList.Free;
              End;
              If InsertAttrIndex < 0 Then
                 InsertAttrIndex := AttributeCount;

              // Jump to property Last.
              // must be String table after!
              SetLength(WriteString, SizeOf(NewAttribute));
              Move(NewAttribute, WriteString[1], Length(WriteString));
              InsertStringToMemoryStream(FAXMLFileStream as TMemoryStream, KeepPosition + (InsertAttrIndex*SizeOf(NewAttribute)), WriteString);

              If NewName < 0 Then
                 AppendStringPool(AName);
              If NewValue < 0 Then
                 AppendStringPool(AValue);

              Result := 0;
              Exit;
           End;
        End;
     End;
  End;
end;

function TAXMLParser.PopBYTEDataFromChunk: Byte;
begin
  Result := 0;
  FAXMLFileStream.Read(Result, 1);
end;

function TAXMLParser.PopDWORDDataFromChunk: Longword;
begin
  Result := 0;
  FAXMLFileStream.Read(Result, 4);
end;

function TAXMLParser.PopNullTerminateStr: String;
var
  BufferChar: Char;
begin
  Result := '';

  While True do
  Begin
     If FAXMLFileStream.Read(BufferChar, 1) <> 1 Then Break;

     Result := Result + BufferChar;
     If BufferChar = #0 Then Break;
  End;
end;

function TAXMLParser.PopSizeAssignedStr(ASize: Longword): String;
begin
  // For Safety
  // If ASize > WorkStrChunk.ChunkSize Then ASize := WorkStrChunk.ChunkSize;
  If ASize > FAXMLFileStream.Size Then ASize := FAXMLFileStream.Size;
  If ASize > CONST_StringResourceLimit Then ASize := CONST_StringResourceLimit; // 주의

  SetLength(Result, ASize);
  FAXMLFileStream.Read(Result[1], ASize);
end;

function TAXMLParser.PopSizeAssignedWideStr(ASize: Longword): WideString;
begin
  // For Safety
  // If ASize > WorkStrChunk.ChunkSize Then ASize := WorkStrChunk.ChunkSize;
  If ASize > FAXMLFileStream.Size Then ASize := FAXMLFileStream.Size;
  If ASize > CONST_StringResourceLimit Then ASize := CONST_StringResourceLimit; // 주의

  SetLength(Result, ASize);
  FAXMLFileStream.Read(Result[1], Length(Result)*SizeOf(Result[1]));
end;

function TAXMLParser.PopUserData(var OutData; DataSize: Longword): Integer;
begin
  FAXMLFileStream.Read(OutData, DataSize);
end;

function TAXMLParser.PopWORDDataFromChunk: Word;
begin
  Result := 0;
  FAXMLFileStream.Read(Result, 2);
end;

function TAXMLParser.ReadTags(AIndex: Integer; AReadFlag: TReadFlag; AFlag: Integer): String;
var
  WorkChunk: PAbstractChunk;
  StrIdx: Longword;
  PrevLevel: Integer;
  LoopVar: Integer;
begin
  Result := '';
  If (AIndex < 0) or (AIndex >= FChunkList.Count) Then Exit;

  WorkChunk := FChunkList[AIndex];
  If (WorkChunk^.ChunkType <> CONST_RType_XMLStartElement) and (WorkChunk^.ChunkType <> CONST_RType_XMLEndElement) Then Exit;

  // process.
  Case AReadFlag of
     rfXMLType:
     Begin
        // Print Option
        If AFlag = 1 Then
        Begin
           Result := StringOfChar(#9, WorkChunk^.ChunkLevel-1);
        End;

        Result := Result + GetTag(AIndex);
     End;

     rfAnalysis:
     Begin
        Result := 'Index : ' + IntToStr(AIndex) + #13#10 + GetTag(AIndex) + #13#10 + GetStream(AIndex) + #13#10;
     End;

     rfTagNameOnly:
     Begin
        // If (WorkChunk^.ChunkType = CONST_RType_XMLEndElement) Then Exit;
        Result := '';
        If (WorkChunk^.ChunkType <> CONST_RType_XMLStartElement) Then Exit;

        FAXMLFileStream.Position := WorkChunk^.ChunkStartPos + 20;
        StrIdx := PopDWORDDataFromChunk;
        Result := GetStringPool(StrIdx);
     End;

     rfTagNamesLevel:
     Begin
        If (WorkChunk^.ChunkType = CONST_RType_XMLEndElement) Then Exit;

        PrevLevel := WorkChunk^.ChunkLevel;
        FAXMLFileStream.Position := WorkChunk^.ChunkStartPos + 20;
        StrIdx := PopDWORDDataFromChunk;
        Result := GetStringPool(StrIdx);

        For LoopVar := AIndex DownTo 0 do
        Begin
           WorkChunk := FChunkList[LoopVar];
           If (WorkChunk^.ChunkType <> CONST_RType_XMLStartElement) or (WorkChunk^.ChunkLevel >= PrevLevel) Then Continue;

           PrevLevel := WorkChunk^.ChunkLevel;
           FAXMLFileStream.Position := WorkChunk^.ChunkStartPos + 20;
           StrIdx := PopDWORDDataFromChunk;
           Result := GetStringPool(StrIdx) + #0 + Result;
        End;
     End;

     rfTagLevel:
     Begin
        Result := IntToStr(WorkChunk^.ChunkLevel);
     End;

     rfTagAttributes:
     Begin
     End;

     rfTagAttributeStringList:
     Begin
        Result := GetTag(AIndex, tofStringList, (AFlag = 1));
     End;
  End;
end;

procedure TAXMLParser.ReleaseLists;
var
  LoopVar: Integer;
begin
  If Assigned(FChunkList) then
  Begin
     For LoopVar := 0 to FChunkList.Count-1 do
     Begin
        FreeMem(PAbstractChunk(FChunkList[LoopVar]));
     End;
     FChunkList.Clear;
  End;

  If Assigned(FNamespaceStack) Then
  Begin
     For LoopVar := 0 to FNamespaceStack.Count-1 do
     Begin
        FreeMem(PNamespaceStack(FNamespaceStack[LoopVar]));
     End;
     FNamespaceStack.Clear;
  End;
end;

procedure TAXMLParser.ReleaseStream;
begin
  If Assigned(FAXMLFileStream) Then FAXMLFileStream.Free;
  FAXMLFileStream := Nil;
end;

function TAXMLParser.ReloadAXMLStream: Integer;
begin
  Result := LoadAXMLStream;
end;

function TAXMLParser.RepleaceTags(AIndex: Integer;
  AReplaceFlag: TReplaceFlag; AValueType: Integer; ASearchValue,
  AReplaceValue: String): Integer;
var
  WorkChunk: PAbstractChunk;
  TagName: Longword;
  AttributeCount: Longword;
  LoopVar: Integer;
  AttributeList: TList;
  AttributeData: PAttributeItem;
  CompareString: String;
  StringIndex: Integer;
  NewValue: Longword;
  KeepPosition: Longword;
begin
  Result := -1;
  If (AIndex < 0) or (AIndex >= FChunkList.Count) Then Exit;

  WorkChunk := FChunkList[AIndex];
  If (WorkChunk^.ChunkType <> CONST_RType_XMLStartElement) and (WorkChunk^.ChunkType <> CONST_RType_XMLEndElement) Then Exit;

  Case AReplaceFlag of
     repfPrepertyValue:
     With WorkChunk^ do
     Begin
        FAXMLFileStream.Position := ChunkStartPos + 8;

        PopDWORDDataFromChunk; // line number
        PopDWORDDataFromChunk; // comment
        PopDWORDDataFromChunk; // namespace
        TagName := PopDWORDDataFromChunk; // name

        Case ChunkType of
           CONST_RType_XMLStartElement:
           Begin
              PopDWORDDataFromChunk; // attribute size
              AttributeCount := PopWORDDataFromChunk; // attribute count
              PopWORDDataFromChunk; // id index
              PopWORDDataFromChunk; // class index
              PopWORDDataFromChunk; // style index

              KeepPosition := FAXMLFileStream.Position;

              AttributeList := TList.Create;
              Try
                 For LoopVar := 1 to AttributeCount do
                 Begin
                    GetMem(AttributeData, SizeOf(TAttributeItem));
                    PopUserData(AttributeData^, SizeOf(TAttributeItem));

                    AttributeList.Add( AttributeData );
                 End;

                 For LoopVar := 0 to AttributeList.Count-1 do
                 Begin
                    AttributeData := AttributeList[LoopVar];

                    // 
                    If AttributeData^.DataType = AValueType Then
                    Begin
                       If AValueType = $03 Then
                       Begin
                          CompareString := GetStringPool(AttributeData^.Data);
                          If CompareString = ASearchValue Then
                          Begin
                             StringIndex := SearchStringPool(AReplaceValue);
                             If StringIndex < 0 Then
                                NewValue := CountStringPool
                             Else
                                NewValue := StringIndex;

                             // Update RAW Value
                             FAXMLFileStream.Position := KeepPosition + (LoopVar * SizeOf(TAttributeItem)) + (2*SizeOf(Longword));
                             FAXMLFileStream.Write(NewValue, Sizeof(Longword));
                             FAXMLFileStream.Position := KeepPosition + (LoopVar * SizeOf(TAttributeItem)) + (4*SizeOf(Longword));
                             FAXMLFileStream.Write(NewValue, Sizeof(Longword));

                             // for debug.
                             //FAXMLFileStream.Position := KeepPosition + (LoopVar * SizeOf(TAttributeItem)) + (4*SizeOf(Longword));
                             //FAXMLFileStream.Read(NewValue, Sizeof(Longword));
                             //WriteLn(IntToStr(NewValue));

                             If StringIndex < 0 Then
                             Begin
                                AppendStringPool(AReplaceValue);
                                // for debug.
                                // writeln( GetStringPool(NewValue) ) ;
                             End;

                             Result := 0;
                          End;
                       End;
                    End;

                    FreeMem(AttributeData);
                 End;
              Finally
                 AttributeList.Free;
              End;
           End;
        End;
     End;
  End;
end;

procedure TAXMLParser.ResetCursor;
begin

end;

function TAXMLParser.SaveAsFile(AFileName: TFileName): Integer;
begin
  Result := -1;

  With (FAXMLFileStream as TMemoryStream) do
  Begin
     If Size <> FFullSize Then
     Begin
        FFullSize := Size;
        FAXMLFileStream.Position := 4;
        FAXMLFileStream.Write(FFullSize, 4);
     End;
     
     // (FAXMLFileStream as TMemoryStream).
     SaveToFile(AFileName);
  End;

  Result := 0;
end;

function TAXMLParser.SearchNamespacePrefix(SearchPrefix: String): Longword;
var
  LoopVar: Integer;
begin
  Result := $FFFFFFFF;
  
  For LoopVar := 0 to FNamespaceStack.Count-1 do
  Begin
     With PNamespaceStack(FNamespaceStack[LoopVar])^ do
     Begin
        If SearchPrefix = GetStringPool(Prefix) Then
        Begin
           Result := Uri;
           Exit;
        End;
     End;
  End;
end;

function TAXMLParser.SearchStringPool(TargetString: String): Integer;
var
  StringPoolMax: Integer;
  LoopVar: Integer;
begin
  Result := -1;
  StringPoolMax := CountStringPool;

  // Search PrevStringItem
  For LoopVar := 0 to StringPoolMax - 1 do
  Begin
     // Found in previous string pool
     If GetStringPool(LoopVar) = TargetString Then
     Begin
        Result := LoopVar;
        Exit;
     End;
  End;
end;

function TAXMLParser.SearchTagInDepth(SearchTag: String): Integer;
begin

end;

function TAXMLParser.TagsCount: Integer;
begin
  Result := FChunkList.Count;
end;

procedure TAXMLParser.TestParser;
var
  LoopVar: Integer;
  WorkChunk: PAbstractChunk;
begin
  //
  For LoopVar := 1 to TagsCount-1 do
  Begin
     WorkChunk := FChunkList[LoopVar];
     If (WorkChunk^.ChunkType = CONST_RType_XMLStartElement) or (WorkChunk^.ChunkType = CONST_RType_XMLEndElement) Then
     Begin
        writeln(StringOfChar(#9, WorkChunk^.ChunkLevel) + GetTag(LoopVar));
        // GetStringPool(LoopVar));
     End;
     // writeln(GetStringPool(LoopVar));
  End;
end;

end.
