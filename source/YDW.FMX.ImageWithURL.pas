{ ❤ 2022 by Kisspeace - https://github.com/Kisspeace }
{ ❤ Part of you-did-well library ❤ ---------------- }
unit YDW.FMX.ImageWithURL;

interface
uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, FMX.Graphics,
  FMX.Objects, YDW.FMX.ImageWithURL.Interfaces;

type

  TImageWithURL = Class(TImage, IImageWithURL)
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

constructor TImageWithURL.Create(AOwner: TComponent);
begin
  inherited;
  FLock := TMREWSync.Create;
  FImageURL := '';
  FImageManager := nil;
end;

destructor TImageWithURL.Destroy;
begin
  AbortLoading;
  WaitForFinish;
  FLock.Free;
  inherited;
end;

function TImageWithURL.GetBitmapIWU: TBitmap;
begin
  Result := Self.Bitmap;
end;

function TImageWithURL.GetImageManager: IImageWithUrlManager;
begin
  Result := FImageManager;
end;

function TImageWithURL.GetImageURL: string;
begin
  FLock.BeginRead;
  try
    Result := FImageURL;
  finally
    FLock.EndRead;
  end;
end;

function TImageWithURL.GetIsLoadingNow: boolean;
begin
  if Assigned(ImageManager) then
    Result := ImageManager.IsLoadingNow(self);
end;

function TImageWithURL.GetOnLoadingFinished: TOnLoadingFinishedEvent;
begin
  Result := FOnLoadingFinished;
end;

procedure TImageWithURL.AbortLoading;
begin
  if Assigned(ImageManager) then
    ImageManager.AbortImage(Self);
end;

procedure TImageWithURL.SetImageManager(const Value: IImageWithUrlManager);
begin
  if Assigned(ImageManager) then
    AbortLoading;

  FImageManager := value;
end;

procedure TImageWithURL.SetImageURL(const Value: string);
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

procedure TImageWithURL.SetOnLoadingFinished(const Value: TOnLoadingFinishedEvent);
begin
  FOnLoadingFinished := Value;
end;

procedure TImageWithURL.WaitForFinish;
begin
  IWUWaitForFinish(Self);
end;

end.
