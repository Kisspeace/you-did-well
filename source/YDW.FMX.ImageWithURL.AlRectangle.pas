{ ❤ 2022 by Kisspeace - https://github.com/Kisspeace }
{ ❤ Part of you-did-well library ❤ ---------------- }
unit YDW.FMX.ImageWithURL.AlRectangle;

interface
uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, FMX.Graphics,
  AlFmxObjects, YDW.FMX.ImageWithURL.Interfaces;

type

  TAlRectangleImageWithURL = Class(TALRectangle, IImageWithURL)
    private
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
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
  End;

implementation

{ TImageWithLinkNetHttp }

constructor TAlRectangleImageWithURL.Create(AOwner: TComponent);
begin
  inherited;
  FLock := TMREWSync.Create;
  FImageURL := '';
  FImageManager := nil;
  Self.Fill.Kind := TBrushKind.Bitmap;
  Self.Stroke.Kind := TBrushKind.None;
end;

destructor TAlRectangleImageWithURL.Destroy;
begin
  AbortLoading;
  WaitForFinish;
  FLock.Free;
  inherited;
end;

function TAlRectangleImageWithURL.GetBitmapIWU: TBitmap;
begin
  Result := Self.Fill.Bitmap.Bitmap;
end;

function TAlRectangleImageWithURL.GetImageManager: IImageWithUrlManager;
begin
  Result := FImageManager;
end;

function TAlRectangleImageWithURL.GetImageURL: string;
begin
  FLock.BeginRead;
  Result := FImageURL;
  FLock.EndRead;
end;

function TAlRectangleImageWithURL.GetIsLoadingNow: boolean;
begin
  if Assigned(ImageManager) then
    Result := ImageManager.IsLoadingNow(self);
end;

function TAlRectangleImageWithURL.GetOnLoadingFinished: TOnLoadingFinishedEvent;
begin
  Result := FOnLoadingFinished;
end;

procedure TAlRectangleImageWithURL.AbortLoading;
begin
  if Assigned(ImageManager) then
    ImageManager.AbortImage(Self);
end;

procedure TAlRectangleImageWithURL.SetImageManager(const Value: IImageWithUrlManager);
begin
  if Assigned(ImageManager) then
    AbortLoading;

  FImageManager := Value;
end;

procedure TAlRectangleImageWithURL.SetImageURL(const Value: string);
begin
  AbortLoading;

  FLock.BeginWrite;
  try
    FImageURL := Value;
    if Assigned(ImageManager) then
      ImageManager.LoadImage(Self);
  finally
    FLock.EndWrite;
  end;
end;

procedure TAlRectangleImageWithURL.SetOnLoadingFinished(const Value: TOnLoadingFinishedEvent);
begin
  FOnLoadingFinished := Value;
end;

procedure TAlRectangleImageWithURL.WaitForFinish;
begin
  IWUWaitForFinish(Self);
end;

end.
