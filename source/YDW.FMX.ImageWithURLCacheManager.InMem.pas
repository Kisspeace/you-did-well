{ ❤ 2022 by Kisspeace - https://github.com/Kisspeace }
{ ❤ Part of you-did-well library ❤ ---------------- }
unit YDW.FMX.ImageWithURLCacheManager.InMem;

interface
uses
  {$IFDEF YDW_DEBUG}
  YDW.Debug,
  {$ENDIF}
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Objects, YDW.FMX.ImageWithURL.Interfaces, FMX.Graphics,
  System.IOUtils, System.Generics.Collections;

type

  TImageWithURLInMemCahceManager = Class(TComponent, IImageWithUrlCacheManager)
    private type
      TCacheItem = Class(TObject)
        public
          Url: string;
          Bitmap: TBitmap;
          Constructor Create(AUrl: string; ASource: TStream);
          Destructor Destroy; override;
      End;

      TCacheItemList = TObjectList<TCacheItem>;
    private
      function CachedItemIndex(const AUrl: string): integer;
    protected
      FLock: TMREWSync;
      FItems: TCacheItemList;
    public
      { ❤ -- Thread safe --- }
      procedure Clear;
      procedure CacheItem(AUrl: string; ASource: TStream);
      procedure LoadFromCache(AUrl: string; ABitmap: TBitmap);
      function IsCached(AUrl: string): boolean;
      { --------------------- }
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
  End;


implementation

{ TImageWithURLInMemCahceManager }


function TImageWithURLInMemCahceManager.CachedItemIndex(const AUrl: string): integer;
var
  I: integer;
begin
  for I := 0 to FItems.Count - 1 do begin
    if FItems[I].Url = AUrl then begin
      Result := I;
      Exit;
    end;
  end;
  Result := -1;
end;

procedure TImageWithURLInMemCahceManager.CacheItem(AUrl: string; ASource: TStream);
var
  LNewItem: TImageWithURLInMemCahceManager.TCacheItem;
begin
  FLock.BeginWrite;
  try
    LNewItem := TImageWithURLInMemCahceManager.TCacheItem.Create(AUrl, ASource);
    FItems.Add(LNewItem);
  finally
    FLock.EndWrite;
  end;
end;

procedure TImageWithURLInMemCahceManager.Clear;
begin
  FLock.BeginWrite;
  try
    FItems.Clear;
  finally
    FLock.EndWrite;
  end;
end;

constructor TImageWithURLInMemCahceManager.Create(AOwner: TComponent);
begin
  inherited;
  FLock := TMREWSync.Create;
  FItems := TCacheItemList.Create;
end;

destructor TImageWithURLInMemCahceManager.Destroy;
begin
  Self.Clear;
  FLock.Free;
  FItems.Free;
  inherited;
end;

function TImageWithURLInMemCahceManager.IsCached(AUrl: string): boolean;
begin
  FLock.BeginRead;
  try
    Result := (CachedItemIndex(AUrl) <> -1);
  finally
    FLock.EndRead;
  end;
end;

procedure TImageWithURLInMemCahceManager.LoadFromCache(AUrl: string;
  ABitmap: TBitmap);
var
  LItemIndex: integer;
  LItem: TImageWithURLInMemCahceManager.TCacheItem;
  LIsMainThread: boolean;
begin
  {$IFDEF YDW_DEBUG} try {$ENDIF}
  FLock.BeginRead;
  try
    LIsMainThread := (TThread.Current.ThreadID = MainThreadId);
    LItemIndex := Self.CachedItemIndex(AUrl);

    if (LItemIndex <> -1) then begin
      LItem := FItems[LItemIndex];

      if LIsMainThread then
        ABitmap.Assign(LItem.Bitmap)
      else
        TThread.Synchronize(TThread.Current, procedure begin
          ABitmap.Assign(LItem.Bitmap);
        end);

    end;
   
  finally
    FLock.EndRead;
  end;
  {$IFDEF YDW_DEBUG}
  Except on E: Exception do begin
    Log('TImageWithURLInMemCahceManager.LoadFromCache();', E);
    Raise E;
  end; end;
  {$ENDIF}
end;

{ TImageWithURLInMemCahceManager.TCahceItem }

constructor TImageWithURLInMemCahceManager.TCacheItem.Create(AUrl: string;
  ASource: TStream);
begin
  Self.Url := AUrl;
  Self.Bitmap := TBitmap.Create;
  Self.Bitmap.LoadFromStream(ASource);
end;

destructor TImageWithURLInMemCahceManager.TCacheItem.Destroy;
begin
  Self.Bitmap.Free;
  inherited;
end;

end.
