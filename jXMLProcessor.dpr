{
  jXMLProcessor is licensed under the
  GNU Lesser General Public License v3.0
}

program jXMLProcessor;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  uJSON in 'uJSON.pas',
  AXMLParser in 'AXMLParser.pas';

const
  NameStr = 'Jkh''s Android XML Processor';
  VersionStr = 'Version 0.1';
  HelpStr = #13#10' > %s --input [TargetFile] --output [TargetFile] --command [Command] --filecommand [Filename] --option [filestream]';
  CommandHelpStr = #13#10'   Command example'#13#10'   {''select-node'':[''application'', ''uses-sdk''], ''target'':''tag|property'', ''command'':''add|modify|remove|print'', ''name'':''value''}';
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
  begin
    If Not Assigned(Worker) Then Exit;

    PrepareFlag := Worker.LoadFromFile(TargetFileName);
    If PrepareFlag < 0 Then
    Begin
       writeln(Format(ErrorCodeStr, [PrepareFlag]));
       Exit;
    End;

    writeln(WorkCommand.toString());
  end;
begin
  writeln(NameStr + ' ' + VersionStr);

  If ParamCount < 1 Then
  Begin
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
     If Length(WorkCommand) <= 1 Then Exit;

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
  Finally
     If Assigned(WorkJSON) Then WorkJSON.Free;
     Worker.Free;
  End;
end.
