{ ❤ 2022 by Kisspeace - https://github.com/Kisspeace }
{ ❤ Part of you-did-well library ❤ ---------------- }
unit YDW.FMX.ImageWithURL.Rectangle;

interface
uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, FMX.Graphics,
  FMX.Objects, YDW.FMX.ImageWithURL.Interfaces;

type

  TRectangleImageWithURL = Class(TRectangle, IImageWithURL)
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

constructor TRectangleImageWithURL.Create(AOwner: TComponent);
begin
  inherited;
  FLock := TMREWSync.Create;
  FImageURL := '';
  FImageManager := nil;
  Self.Fill.Kind := TBrushKind.Bitmap;
  Self.Stroke.Kind := TBrushKind.None;
end;

destructor TRectangleImageWithURL.Destroy;
begin
  AbortLoading;
  WaitForFinish;
  FLock.Free;
  inherited;
end;

function TRectangleImageWithURL.GetBitmapIWU: TBitmap;
begin
  Result := Self.Fill.Bitmap.Bitmap;
end;

function TRectangleImageWithURL.GetImageManager: IImageWithUrlManager;
begin
  Result := FImageManager;
end;

function TRectangleImageWithURL.GetImageURL: string;
begin
  FLock.BeginRead;
  Result := FImageURL;
  FLock.EndRead;
end;

function TRectangleImageWithURL.GetIsLoadingNow: boolean;
begin
  if Assigned(ImageManager) then
    Result := ImageManager.IsLoadingNow(self);
end;

function TRectangleImageWithURL.GetOnLoadingFinished: TOnLoadingFinishedEvent;
begin
  Result := FOnLoadingFinished;
end;

procedure TRectangleImageWithURL.AbortLoading;
begin
  if Assigned(ImageManager) then
    ImageManager.AbortImage(Self);
end;

procedure TRectangleImageWithURL.SetImageManager(const Value: IImageWithUrlManager);
begin
  if Assigned(ImageManager) then
    AbortLoading;

  FImageManager := value;
end;

procedure TRectangleImageWithURL.SetImageURL(const Value: string);
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

procedure TRectangleImageWithURL.SetOnLoadingFinished(const Value: TOnLoadingFinishedEvent);
begin
  FOnLoadingFinished := Value;
end;

procedure TRectangleImageWithURL.WaitForFinish;
begin
  IWUWaitForFinish(Self);
end;

end.
