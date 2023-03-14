{ ❤ 2022 by Kisspeace - https://github.com/Kisspeace }
{ ❤ Part of you-did-well library ❤ ---------------- }
unit YDW.FMX.ImageWithURLCacheManager;

interface
uses
  {$IFDEF YDW_DEBUG}
  YDW.Debug,
  {$ENDIF}
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Objects, YDW.FMX.ImageWithURL.Interfaces, FMX.Graphics,
  System.IOUtils, System.Hash;

type

  TImageWithURLCahceManager = Class(TComponent, IImageWithUrlCacheManager)
    private const
      SHA_VERSION = THashSHA2.TSHA2Version.SHA256;
    private
      function GetSavePath: string;
      procedure SetSavePath(const value: string);
      function GetLoadPaths: TArray<string>;
      procedure SetLoadPaths(const value: TArray<string>);
      function GetDefaultFilenameExtension: string;
      procedure SetDefaultFilenameExtension(const Value: string);
      function GetSupportedExts: TArray<string>;
      procedure SetSupportedExts(const Value: TArray<string>);
      function GetSyncBitmapLoadFromFile: boolean;
      procedure SetSyncBitmapLoadFromFile(const Value: boolean);
    protected
      FLock: TMREWSync;
      FSavePath: string;
      FLoadPaths: TArray<string>;
      FSupportedExts: TArray<string>;
      FDefaultFilenameExtension: string;
      FSyncBitmapLoadFromFile: boolean;
      function SupportsExt(AExt: string): boolean;
      function AssetFilenameFromUrl(AUrl: string): string; virtual;
    public
      procedure SetSaveAndLoadPath(APath: string);
      procedure AddLoadPath(APath: string);
      procedure AddLoadPaths(APaths: TArray<string>);
      procedure CacheItem(AUrl: string; ASource: TStream);
      procedure LoadFromCache(AUrl: string; ABitmap: TBitmap);
      function IsCached(AUrl: string): boolean;
      function FindCachedFilename(AUrl: string): string;
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
    published
      property SavePath: string read GetSavePath write SetSavePath;
      property LoadPaths: TArray<string> read GetLoadPaths write SetLoadPaths;
      property DefaultFilenameExtension: string read GetDefaultFilenameExtension write SetDefaultFilenameExtension;
      property SupportedExts: TArray<string> read GetSupportedExts write SetSupportedExts;
      property SyncBitmapLoadFromFile: boolean read GetSyncBitmapLoadFromFile write SetSyncBitmapLoadFromFile default True;
  End;


implementation

{ TImageWithURLCahceManager }

procedure TImageWithURLCahceManager.AddLoadPath(APath: string);
begin
  FLock.BeginWrite;
  FLoadPaths := FLoadPaths + [APath];
  FLock.EndWrite;
end;

procedure TImageWithURLCahceManager.AddLoadPaths(APaths: TArray<string>);
begin
  FLock.BeginWrite;
  FLoadPaths := FLoadPaths + APaths;
  FLock.EndWrite;
end;

function TImageWithURLCahceManager.AssetFilenameFromUrl(AUrl: string): string;
var
  LExt: string;
begin
  LExt := TPath.GetExtension(AUrl);
  Result := System.Hash.THashSHA2.GetHashString(AUrl, SHA_VERSION);

  if not SupportsExt(LExt) then
    LExt := self.DefaultFilenameExtension;

  Result := TPath.ChangeExtension(Result, LExt);
end;

procedure TImageWithURLCahceManager.CacheItem(AUrl: string; ASource: TStream);
var
  LSavePath: string;
  LFilename: string;
begin
  LSavePath := Self.SavePath;

  // Create dir if not exists
  if not DirectoryExists(LSavePath) then
    ForceDirectories(LSavePath);

  LFilename := AssetFilenameFromUrl(AUrl);
  LFilename := TPath.Combine(LSavePath, LFilename);

  try
    (ASource As TMemoryStream).SaveToFile(LFilename);
  except
  end;
end;

constructor TImageWithURLCahceManager.Create(AOwner: TComponent);
begin
  inherited;
  FLock := TMREWSync.Create;
  FLoadPaths := [];
  FSupportedExts := ['.jpg', '.jpeg', '.png', '.gif', '.webp'];
  FDefaultFilenameExtension := '.jpg';
  FSavePath := '';
  FSyncBitmapLoadFromFile := True;
end;

destructor TImageWithURLCahceManager.Destroy;
begin
  FLock.Free;
  inherited;
end;

function TImageWithURLCahceManager.FindCachedFilename(AUrl: string): string;
var
  I: integer;
  LPaths: TArray<String>;
  LAssetFilename: string;
  LFullFilename: string;
begin
  Result := ''; // default empty (not found)
  LPaths := Self.LoadPaths;
  LAssetFilename := Self.AssetFilenameFromUrl(AUrl);

  for I := Low(LPaths) to High(LPaths) do begin
    LFullFilename := Tpath.Combine(LPaths[I], LAssetFilename);
    if FileExists(LFullFilename) then begin
      Result := LFullFilename;
      exit;
    end;
  end;
end;

function TImageWithURLCahceManager.GetSyncBitmapLoadFromFile: boolean;
begin
  FLock.BeginRead;
  Result := FSyncBitmapLoadFromFile;
  FLock.EndRead;
end;

function TImageWithURLCahceManager.GetDefaultFilenameExtension: string;
begin
  FLock.BeginRead;
  Result := Self.FDefaultFilenameExtension;
  FLock.EndRead;
end;

function TImageWithURLCahceManager.GetLoadPaths: TArray<string>;
begin
  FLock.BeginRead;
  Result := FLoadPaths;
  FLock.EndRead;
end;

function TImageWithURLCahceManager.GetSavePath: string;
begin
  FLock.BeginRead;
  Result := FSavePath;
  FLock.EndRead;
end;

function TImageWithURLCahceManager.GetSupportedExts: TArray<string>;
begin
  FLock.BeginRead;
  Result := Self.FSupportedExts;
  FLock.EndRead;
end;

function TImageWithURLCahceManager.IsCached(AUrl: string): boolean;
begin
  Result := (not self.FindCachedFilename(AUrl).IsEmpty);
end;

procedure TImageWithURLCahceManager.LoadFromCache(AUrl: string;
  ABitmap: TBitmap);
var
  LFullFilename: string;
  LIsMainThread: boolean;
begin
  LFullFilename := Self.FindCachedFilename(AUrl);
  if (not LFullFilename.IsEmpty) then begin
    LIsMainThread := (TThread.Current.ThreadID = MainThreadId);
    {$IFDEF YDW_DEBUG} try {$ENDIF}
    if LIsMainThread then begin
      ABitmap.LoadFromFile(LFullFilename);
    end else begin

      if SyncBitmapLoadFromFile then begin

        TThread.Synchronize(TThread.Current, procedure
        begin
          ABitmap.LoadFromFile(LFullFilename);
        end);

      end else begin

        var LBufBmp: TBitmap;
        LBufBmp := TBitmap.Create;

        try
          LBufBmp.LoadFromFile(LFullFilename);
          TThread.Synchronize(TThread.Current, procedure begin
            ABitmap.Assign(LBufBmp);
          end);
        finally
          LBufBmp.free;
        end;

      end;

    end;
    {$IFDEF YDW_DEBUG}
    Except on E: Exception do begin
      Log('TImageWithURLCahceManager.LoadFromCache(); SyncBitmapLoadFromFile: ' +
          Booltostr(SyncBitmapLoadFromFile, True), E);
      raise;
    end; end;
    {$ENDIF}
  end;
end;

procedure TImageWithURLCahceManager.SetSyncBitmapLoadFromFile(
  const Value: boolean);
begin
  FLock.BeginWrite;
  FSyncBitmapLoadFromFile := Value;
  FLock.EndWrite;
end;

procedure TImageWithURLCahceManager.SetDefaultFilenameExtension(
  const Value: string);
begin
  FLock.BeginWrite;
  FDefaultFilenameExtension := value;
  FLock.EndWrite;
end;

procedure TImageWithURLCahceManager.SetLoadPaths(const value: TArray<string>);
begin
  FLock.BeginWrite;
  FLoadPaths := value;
  FLock.EndWrite;
end;

procedure TImageWithURLCahceManager.SetSaveAndLoadPath(APath: string);
begin
  FLock.BeginWrite;
  FSavePath := APath;
  FLoadPaths := [APath];
  FLock.EndWrite;
end;

procedure TImageWithURLCahceManager.SetSavePath(const value: string);
begin
  FLock.BeginWrite;
  FSavePath := value;
  FLock.EndWrite;
end;

procedure TImageWithURLCahceManager.SetSupportedExts(
  const Value: TArray<string>);
begin
  FLock.BeginWrite;
  FSupportedExts := value;
  FLock.EndWrite;
end;

function TImageWithURLCahceManager.SupportsExt(AExt: string): boolean;
var
  I: integer;
begin
  FLock.BeginRead;
  try
    Result := False;
    for I := 0 to High(FSupportedExts) do begin
      if ( FSupportedExts[I].ToUpper = AExt.ToUpper ) then begin
        Result := True;
        exit;
      end;
    end;
  finally
    FLock.EndRead;
  end;
end;

end.
