{ ❤ 2022 by Kisspeace - https://github.com/Kisspeace }
{ ❤ Part of you-did-well library ❤ ---------------- }
unit YDW.FMX.ImageWithURL.Module;

interface
uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, FMX.Graphics,
  FMX.Objects, YDW.FMX.ImageWithURL.Interfaces, FMX.StdCtrls;

type

  { TImageWithUrlModule }
  TIWUModule = Class(TComponent, IImageWithUrl)
    private
      FBitmap: TBitmap;
      FImageURL: string;
      FImageManager: IImageWithURLManager;
      FLock: TMREWSync;
      FOnLoadingFinished: TOnLoadingFinishedEvent;
    protected
      function GetImageURL: string;
      procedure SetImageURL(const Value: string);
      function GetImageManager: IImageWithUrlManager;
      procedure SetImageManager(const Value: IImageWithUrlManager);
      function GetIsLoadingNow: boolean;
      function GetBitmapIWU: TBitmap;
      function GetOnLoadingFinished: TOnLoadingFinishedEvent;
      procedure SetOnLoadingFinished(const Value: TOnLoadingFinishedEvent);
    public
      procedure AbortLoading;
      procedure WaitForFinish;
      property ImageURL: string read GetImageURL write SetImageURL;
      property ImageManager: IImageWithUrlManager read GetImageManager write SetImageManager;
      property IsLoadingNow: boolean read GetIsLoadingNow;
      property BitmapIWU: TBitmap read GetBitmapIWU;
      property OnLoadingFinished: TOnLoadingFinishedEvent read GetOnLoadingFinished write SetOnLoadingFinished;
      constructor Create(AOwner: TComponent); overload; override;
      constructor Create(AOwner: TComponent; ATargetBitmap: TBitmap); overload; virtual;
      destructor Destroy; override;
  End;

  function IWUModule(const AModuleOwner: TComponent): TIWUModule;

implementation

function IWUModule(const AModuleOwner: TComponent): TIWUModule;
var
  I: integer;
begin
  for I := 0 to AModuleOwner.ComponentCount - 1 do begin
    if ( AModuleOwner.Components[I] is TIWUModule ) then begin
      Result := TIWUModule(AModuleOwner.Components[I]);
      Exit;
    end;
  end;
  Result := nil;
end;

{ TIWUModule }

constructor TIWUModule.Create(AOwner: TComponent);
begin
  inherited;
  FLock := TMREWSync.Create;
  FImageURL := '';
  FImageManager := nil;

  if Supports(AOwner, IBitmapObject) then begin
    FBitmap := (AOwner as IBitmapObject).Bitmap;
  end else if ( AOwner is TShape ) then begin
    FBitmap := (AOwner as TShape).Fill.Bitmap.Bitmap;
  end else if ( AOwner is TImageControl ) then begin
    FBitmap := TImageControl(AOwner).Bitmap;
  end;
end;

constructor TIWUModule.Create(AOwner: TComponent; ATargetBitmap: TBitmap);
begin
  Create(AOwner);
  FBitmap := ATargetBitmap;
end;

destructor TIWUModule.Destroy;
begin
  AbortLoading;
  WaitForFinish;
  FLock.Free;
  inherited;
end;

function TIWUModule.GetBitmapIWU: TBitmap;
begin
  Result := Self.FBitmap;
end;

function TIWUModule.GetImageManager: IImageWithUrlManager;
begin
  Result := FImageManager;
end;

function TIWUModule.GetImageURL: string;
begin
  FLock.BeginRead;
  Result := FImageURL;
  FLock.EndRead;
end;

function TIWUModule.GetIsLoadingNow: boolean;
begin
  if Assigned(ImageManager) then
    Result := ImageManager.IsLoadingNow(self);
end;

function TIWUModule.GetOnLoadingFinished: TOnLoadingFinishedEvent;
begin
  Result := FOnLoadingFinished;
end;

procedure TIWUModule.AbortLoading;
begin
  if Assigned(ImageManager) then
    ImageManager.AbortImage(Self);
end;

procedure TIWUModule.SetImageManager(const Value: IImageWithUrlManager);
begin
  if Assigned(ImageManager) then
    AbortLoading;

  FImageManager := value;
end;

procedure TIWUModule.SetImageURL(const Value: string);
begin
  AbortLoading;

  FLock.BeginWrite;
  try
    FImageURL := value;
    if Assigned(ImageManager) then
      ImageManager.LoadImage(Self);
  finally
    FLock.EndWrite;
  end;
end;

procedure TIWUModule.SetOnLoadingFinished(const Value: TOnLoadingFinishedEvent);
begin
  FOnLoadingFinished := value;
end;

procedure TIWUModule.WaitForFinish;
begin
  IWUWaitForFinish(Self);
end;

end.
