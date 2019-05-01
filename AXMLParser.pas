{
  jXMLProcessor is licensed under the
  GNU Lesser General Public License v3.0
}

unit AXMLParser;

interface

uses
  SysUtils, Classes;

type
  TStringChunk = record
     ChunkType: Longword;
     ChunkSize: Longword;
     StringCount: Longword;
     StyleCount: Longword;
     Unknown1: Longword;
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

  TAbstractChunk = record
    ChunkType: Longword;
    ChunkSize: Longword;

    ChunkEndPos: Longword; // for virtual
  end;
  PAbstractChunk = ^TAbstractChunk;

  TAXMLParser = class
  private
    FAXMLFileStream: TStream;

    FHeader: Longword;
    FFullSize: Longword;
    FChunkList: TList;

    procedure ReleaseStream;
    procedure ReleaseLists;
  protected
    function LoadAXMLStream: Integer;
  public
    constructor Create;
    destructor Destroy;

    function LoadFromFile(AFileName: TFileName; AOnMemory: Boolean = True): Integer;
  end;

implementation

const
  CONST_AXMLHeader = $080003;

constructor TAXMLParser.Create;
begin
  inherited;

  FAXMLFileStream := Nil;

  FChunkList := TList.Create;
end;

destructor TAXMLParser.Destroy;
begin
  ReleaseLists;
  FChunkList.Free;

  ReleaseStream;

  inherited;
end;

function TAXMLParser.LoadAXMLStream: Integer;
var
  LoopVar: Integer;
  ReadSize: Integer;
  ParseChunk: Longword;

  procedure ReadChunk;
  const
    ChunkHeaderSize = 8;
  var
    NewChunk: PAbstractChunk;
  begin
    GetMem(NewChunk, SizeOf(TAbstractChunk));
    FChunkList.Add(NewChunk);

    NewChunk^.ChunkType := ParseChunk;
    // Next is ChunkSize
    FAXMLFileStream.Read(NewChunk^.ChunkSize, 4);
    FAXMLFileStream.Position := FAXMLFileStream.Position + NewChunk^.ChunkSize - ChunkHeaderSize;
  end;

begin
  Result := -10;
  If Not Assigned(FAXMLFileStream) Then Exit;

  // Check Size
  Result := -11;
  If FAXMLFileStream.Size < 8 Then Exit;

  FAXMLFileStream.Position := 0;

  // Check Magic Number
  FAXMLFileStream.Read(FHeader, 4);
  Result := -100;
  If FHeader <> CONST_AXMLHeader Then Exit;

  // Prepare Safe Size Position
  FAXMLFileStream.Read(FFullSize, 4);

  // ReadChunk
  While True do
  Begin
     ReadSize := FAXMLFileStream.Read(ParseChunk, 4);
     // if not exists..
     If (ReadSize < 4) or (FFullSize <= FAXMLFileStream.Position) Then Break;

     Case ParseChunk of
        $001C0001, // String Chunk
        $00080180, // ResourceId Chunk

        $00100100, // Start Namespace Chunk
        $00100101, // End Namespace Chunk

        $00100102, // Start Tag Chunk
        $00100103, // End Tag Chunk

        $00100104  // Text Chunk

        : ReadChunk;
     End;
  End;
end;

function TAXMLParser.LoadFromFile(AFileName: TFileName; AOnMemory: Boolean): Integer;
begin
  Result := -1;
  ReleaseStream;
  ReleaseLists;

  Result := -2;
  If Not FileExists(AFileName) Then Exit;

  If Not AOnMemory Then
  Begin
     FAXMLFileStream := TFileStream.Create(AFileName, fmOpenReadWrite);
  End
  Else
  Begin
     FAXMLFileStream := TMemoryStream.Create;
     (FAXMLFileStream as TMemoryStream).LoadFromFile(AFileName);
  End;

  Result := LoadAXMLStream;
end;

procedure TAXMLParser.ReleaseLists;
var
  LoopVar: Integer;
begin
  For LoopVar := 0 to FChunkList.Count-1 do
  Begin
     FreeMem(PAbstractChunk(FChunkList[LoopVar]));
  End;
  FChunkList.Clear;
end;

procedure TAXMLParser.ReleaseStream;
begin
  If Assigned(FAXMLFileStream) Then FAXMLFileStream.Free;
  FAXMLFileStream := Nil;
end;

end.
