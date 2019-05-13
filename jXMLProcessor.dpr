{
  jXMLProcessor is licensed under the
  GNU Lesser General Public License v3.0
}

program jXMLProcessor;

{$IFDEF MSWINDOWS}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  SysUtils,
  Classes,
  uJSON in 'uJSON.pas',
  AXMLParser in 'AXMLParser.pas';

const
  NameStr = 'Jkh''s Android XML Processor';
  VersionStr = 'Version 0.1';
  HelpStr = #13#10' > %s --input [TargetFile] --output [TargetFile] --command [Command] --filecommand [Filename] --option [filestream]';
  CommandHelpStr = #13#10'   Command example'#13#10'   {''select-node'':[''application'', ''uses-sdk''],'#13#10'    ''command'':''make-tag|rename-tag|remove-tag|make-property|rename-property|remove-property|change-property-value|print'','#13#10'    ''taget'':''...'', ''name'':''...'', ''value'':''...'', ''option'':''...''';
  ErrorCodeStr = 'Error code : %d';

var
  Worker: TAXMLParser;
  ParamLoop: Integer;
  LoopVar: Integer;

  NextCommand: String;

  TargetFileName,
  OutputFileName: TFileName;
  WorkCommand: String;
  WorkJSON: TZAbstractObject;
  
  function LoadFile(AFileName: TFileName): String;
  var
    Worker: TFileStream;
  begin
    Result := '';
    If Not FileExists(AFileName) Then Exit;

    Worker := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
    Try
       SetLength(Result, Worker.Size);
       Worker.Read(Result[1], Worker.Size);
    Finally
       Worker.Free;
    End;
  end;

  procedure ProcessCommand(const WorkCommand: TJSONObject);
  var
    PrepareFlag: Integer;
    SelectArray: TJSONArray;
    StartIdx, EndIdx: Integer;
    LevelCandidate,
    LevelIdx: Integer;
    LineStr,
    TargetMatch,
    MatchStr: String;
    MatchCount,
    LoopVar: Integer;
    MatchTestList: TStringList;
    ParsedCommand: String;
  begin
    If Not Assigned(Worker) Then Exit;

    PrepareFlag := Worker.LoadFromFile(TargetFileName);
    If PrepareFlag < 0 Then
    Begin
       writeln(Format(ErrorCodeStr, [PrepareFlag]));
       Exit;
    End;

    ParsedCommand := Trim(WorkCommand.getStringSlient('command'));
    // not work.
    If ParsedCommand = '' Then Exit;
    // writeln(WorkCommand.toString());

    LevelIdx := -1;
    StartIdx := 0;
    MatchCount := 0;
    EndIdx := Worker.TagsCount-1;

    // first. select nodes.
    SelectArray := WorkCommand.getJSONArraySlient('select-node');
    If Assigned(SelectArray) and (SelectArray.toString <> 'null') and (SelectArray.length > 0) Then
    Begin
       TargetMatch := SelectArray.getString(0);
       For LoopVar := 1 to SelectArray.length-1 do
          TargetMatch := TargetMatch + #0 + SelectArray.getString(LoopVar);
       TargetMatch := LowerCase(TargetMatch);

       For LoopVar := StartIdx to EndIdx do
       Begin
          MatchStr := Worker.ReadTags(LoopVar, rfTagNamesLevel);
          If MatchStr = '' Then Continue;

          If TargetMatch = LowerCase(MatchStr) Then
          Begin
             // Found Start.
             MatchStr := Worker.ReadTags(LoopVar, rfTagLevel);
             If MatchStr <> '' Then
             Begin
                TryStrToInt(MatchStr, LevelIdx);
                StartIdx := LoopVar;
             End;

             Break;
          End;
       End;

       If (LevelIdx >= 0) Then
       Begin
          For LoopVar := StartIdx+1 to EndIdx do
          Begin
             MatchStr := Worker.ReadTags(LoopVar, rfTagLevel);
             If MatchStr = '' Then Continue;

             TryStrToInt(MatchStr, LevelCandidate);
             If LevelCandidate <= LevelIdx Then
             Begin
                EndIdx := LoopVar;
                Break;
             End;
          End;
       End;
    End;

    // now start work!
    If (ParsedCommand = 'print') Then
    Begin
       For LoopVar := StartIdx to EndIdx do
       Begin
          LineStr := Worker.ReadTags(LoopVar, rfXMLType, 1);
          If LineStr = '' Then Continue;

          writeln(LineStr);
       End;
    End
    Else If ParsedCommand = 'make-property' Then
    Begin
       If WorkCommand.isNull('name') or WorkCommand.isNull('value') Then
       Begin
          writeln(' - name,value pair not exists');
          Exit;
       End;

       For LoopVar := StartIdx to EndIdx do
       Begin
          If Worker.MakeTags(LoopVar, mkfProperty, $03, WorkCommand.getStringSlient('name'), WorkCommand.getStringSlient('value')) = 0 Then Exit;
       End;
       ExitCode := -1;
    End
    Else If ParsedCommand = 'change-property-value' Then
    Begin
       If WorkCommand.isNull('target') or WorkCommand.isNull('value') Then
       Begin
          writeln(' - target,value pair not exists');
          Exit;
       End;

       // for debug.
       // Worker.AppendStringPool('TestAppending');

       For LoopVar := StartIdx to EndIdx do
       Begin
          If Worker.RepleaceTags(LoopVar, repfPrepertyValue, $03, WorkCommand.getStringSlient('target'), WorkCommand.getStringSlient('value')) = 0 Then Exit;
       End;
       ExitCode := -1;
    End;

  end;
begin
  If ParamCount < 1 Then
  Begin
     writeln(NameStr + ' ' + VersionStr);

     writeln(Format(HelpStr, [ExtractFileName(ParamStr(0))]));
     writeln(CommandHelpStr);

     Exit;
  End;

  NextCommand := '';
  ParamLoop := 1;

  TargetFileName := '';
  OutputFileName := '';
  WorkCommand := '';

  While (ParamCount >= ParamLoop) do
  Begin
     If NextCommand <> '' Then
     Begin
        If (NextCommand = '--input') or (NextCommand = '-i') Then
        Begin
           TargetFileName := ParamStr(ParamLoop);
        End
        Else If (NextCommand = '--output') or (NextCommand = '-o') Then
        Begin
           OutputFileName := ParamStr(ParamLoop);
        End
        Else If (NextCommand = '--command') or (NextCommand = '-c') Then
        Begin
           WorkCommand := ParamStr(ParamLoop);
        End
        Else If (NextCommand = '--filecommand') or (NextCommand = '-f') Then
        Begin
           WorkCommand := LoadFile(ParamStr(ParamLoop));
        End;
        {
        // no more support FileStream.
        Else If (NextCommand = '--option') or (NextCommand = '-p') Then
        Begin
           If ParamStr(ParamLoop) = 'filestream' Then
           Begin
              WorkOnFile := True;
           End;
        End;
        }

        NextCommand := '';
     End;

     If Copy(ParamStr(ParamLoop), 1, 1) = '-' Then
        NextCommand := LowerCase(ParamStr(ParamLoop));

     Inc(ParamLoop);
  End;

  If Not FileExists(TargetFileName) Then
  Begin
     writeln('No such file ' + TargetFileName);
     Exit;
  End;

  Worker := TAXMLParser.Create;
  WorkJSON := Nil;
  Try
     WorkCommand := Trim(WorkCommand);
     If (Length(WorkCommand) <= 1) Then
     Begin
        // Default command is print.
        WorkCommand := '{''command'':''print''}';
     End;

     Case WorkCommand[1] of
        '{':
        Begin
           WorkJSON := TJSONObject.create(WorkCommand);
           ProcessCommand(WorkJSON as TJSONObject);
        End;
        '[':
        Begin
           WorkJSON := TJSONArray.create(WorkCommand);
           For LoopVar := 0 to (WorkJSON as TJSONArray).length -1 do
              Try
                 ProcessCommand( (WorkJSON as TJSONArray).get(LoopVar) as TJSONObject );
              Except
                 Break;
              End;
        End;
     End;

     If OutputFileName <> '' Then
     Begin
        Worker.SaveAsFile(OutputFileName);
     End;
  Finally
     If Assigned(WorkJSON) Then WorkJSON.Free;
     Worker.Free;
  End;
end.
