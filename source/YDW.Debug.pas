{ ❤ 2022 by Kisspeace - https://github.com/Kisspeace }
{ ❤ Part of you-did-well library ❤ ---------------- }
unit YDW.Debug;

interface
uses
  Classes, Sysutils, System.IOUtils, Types, DateUtils;

type

  TLogFile = class(TObject)
    public const
      DEFAULT_FILENAME: string = 'you-did-well-debug-log.txt';
    private
      FLock: TMREWSync;
      FFilename: string;
      function GetFilename: string;
      procedure SetFilename(const Value: string);
    public
      property Filename: string read GetFilename write SetFilename;
      procedure Log(AText: string; AExcept: Exception); overload;
      procedure Log(AText: string); overload;
      constructor Create;
      destructor Destroy; override;
  end;

  TMREWSyncWithLog = Class(TObject) { for debug }
    private
      FLock: TMREWSync;
      {$IFDEF MSWINDOWS}
      function GetRevisionLevel: Cardinal;
      {$ENDIF}
      procedure LogMsg(AMsg: string);
      function L(ALine: integer): string;
    public
      LogFile: TLogFile;
      Name: string;
      constructor Create;
      destructor Destroy; override;
      procedure BeginRead(ALine: integer = -1);
      procedure EndRead(ALine: integer = -1);
      function BeginWrite(ALine: integer = -1): Boolean;
      procedure EndWrite(ALine: integer = -1);
      {$IFDEF MSWINDOWS}
      property RevisionLevel: Cardinal read GetRevisionLevel;
      {$ENDIF}
  End;

  {$IFDEF YDW_DEBUG}
  var YDWLog: TLogFile;

  procedure Log(AText: string; AExcept: Exception); overload;
  procedure Log(AText: string); overload;
  {$ENDIF}

implementation

{$IFDEF YDW_DEBUG}
procedure Log(AText: string; AExcept: Exception);
begin
  YDWLog.Log(AText, AExcept);
end;

procedure Log(AText: string); overload;
begin
  YDWLog.Log(AText);
end;
{$ENDIF}

{ TLogFile }

procedure WriteToFile(AFileName, Atext: string);
var
  F: TFileStream;
  Encoding: TEncoding;
  LBytes: TBytes;
  LDir: string;
begin
  try
    LDir := Tpath.GetDirectoryName(AFilename);
    if Not DirectoryExists(LDir) then
      CreateDir(LDir);

    if FileExists(AFilename) then
      F := TFileStream.Create(AFilename, FmOpenWrite)
    else
      F := TFileStream.Create(AFilename, FmCreate);

    Encoding := TEncoding.Default;
    LBytes := Encoding.GetBytes(AText);
    F.Position := F.Size;
    F.Write(LBytes, 0, Length(LBytes));
  finally
    F.Free;
  end;
end;

constructor TLogFile.Create;
begin
  FLock := TMREWSync.Create;
  FFilename := TLogFile.DEFAULT_FILENAME;
end;

destructor TLogFile.Destroy;
begin
  FLock.Free;
  inherited;
end;

function TLogFile.GetFilename: string;
begin
  FLock.BeginRead;
  Result := FFilename;
  FLock.EndRead;
end;

procedure TLogFile.Log(AText: string; AExcept: Exception);
begin
  Self.Log(AText + ' - {EXCEPTION!} - ' + AExcept.ToString);
end;

procedure TLogFile.Log(AText: string);
var
  LText: string;

begin
  LText := '[ ' + Now().ToString + ' ]: ';

  if TThread.Current.ThreadID = MainThreadId then
    LText := LText + 'MainThread, '
  else
    LText := LText + 'Thread(' + TThread.Current.ThreadID.ToString + ') ';

  LText := LText + AText + SLineBreak;

  FLock.BeginWrite;
  try
    WriteToFile(FFilename, LText);
  finally
    FLock.EndWrite;
  end;
end;

procedure TLogFile.SetFilename(const Value: string);
begin
  FLock.BeginWrite;
  FFIlename := Value;
  FLock.EndWrite;
end;

{ TMREWSyncWithLog }

procedure TMREWSyncWithLog.BeginRead(ALine: integer);
begin
  logMsg('BeginRead' + L(ALine));
  Self.FLock.BeginRead;
end;

function TMREWSyncWithLog.BeginWrite(ALine: integer): Boolean;
begin
  logMsg('BeginWrite' + L(ALine));
  Self.FLock.BeginWrite;
end;

constructor TMREWSyncWithLog.Create;
begin
  FLock := TMREWSync.Create;
  {$IFDEF YDW_DEBUG} Self.LogFile := YDWLog; {$ENDIF}
  Self.Name := Random(Integer.MaxValue).ToHexString;
end;

destructor TMREWSyncWithLog.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TMREWSyncWithLog.EndRead(ALine: integer);
begin
  logMsg('EndRead' + L(ALine));
  FLock.EndRead;
end;

procedure TMREWSyncWithLog.EndWrite(ALine: integer);
begin
  logMsg('EndWrite' + L(ALine));
  FLock.EndWrite;
end;

{$IFDEF MSWINDOWS}
function TMREWSyncWithLog.GetRevisionLevel: Cardinal;
begin
  Result := FLock.RevisionLevel;
end;
{$ENDIF}

function TMREWSyncWithLog.L(ALine: integer): string;
var
  LLineStr: string;
begin
  if Aline = -1 then
    LLineStr := 'U'
  else
    LLineStr := ALine.ToString;
  Result := '(' + LLineStr + ')';
end;

procedure TMREWSyncWithLog.LogMsg(AMsg: string);
begin
   Self.LogFile.Log('MREWSync: ' + Self.Name + ' - ' + AMsg);
end;

{$IFDEF YDW_DEBUG}
initialization
begin
  YDWLog := TLogFile.Create;
  {$IFDEF ANDROID}
  YDWLog.FFilename := TPath.Combine(TPath.GetDocumentsPath, YDWLog.FFilename);
  {$ELSE IF MSWINDOWS}
  YDWLog.FFilename := TPath.Combine(ExtractFilePath(ParamStr(0)), YDWLog.FFilename);
  {$ENDIF}
//  Log('/-------------------------|START|----------------------\');
end;
{$ENDIF}
end.




