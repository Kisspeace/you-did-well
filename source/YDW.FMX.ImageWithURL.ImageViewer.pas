{ ❤ 2022 by Kisspeace - https://github.com/Kisspeace }
{ ❤ Part of you-did-well library ❤ ---------------- }
unit YDW.FMX.ImageWithURL.ImageViewer;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.Layouts, FMX.ExtCtrls, FMX.Types, FMX.Graphics,
  YDW.FMX.ImageWithURLManager, YDW.FMX.ImageWithURL.Interfaces,
  YDW.FMX.ImageWithURLCacheManager,
  YDW.FMX.ImageWithURL.Module, YDW.FMX.ImageWithURL;

type

  TZoomingEvent = reference to procedure (AValue: Integer);

  TImageWithUrlViewer = Class(TImageViewer, IImageWithUrl)
    private
      FImageURL: string;
      FImageManager: IImageWithURLManager;
      FLock: TMREWSync;
      FOnLoadingFinished: TOnLoadingFinishedEvent;
    { ❤ --------------- }
      FEnableZoomingOutOfFit: boolean;
      FBestFitOnDoubleTap: boolean;
      FInvertMouseWheel: boolean;
      FLastZoomDist: Integer;
      FZoomFactor: Single;
      FMoveFactor: Single;
      FNeedToMove: boolean;
      FMovePoint: TPointF;
      FPrevContentBounds: TSizeF;
      FOnZoomBegin: TZoomingEvent;
      procedure FitWithSize(AWidth, AHeight: integer);
    protected
      function GetImageURL: string;
      procedure SetImageURL(const Value: string);
      function GetImageManager: IImageWithUrlManager;
      procedure SetImageManager(const Value: IImageWithUrlManager);
      function GetIsLoadingNow: boolean;
      function GetBitmapIWU: TBitmap;
      function GetOnLoadingFinished: TOnLoadingFinishedEvent;
      procedure SetOnLoadingFinished(const Value: TOnLoadingFinishedEvent);
    { ❤ --------------- }
      procedure FinishMove; virtual;
      procedure DoZoom(AValue: Integer); virtual;
      procedure DoOnZoomBegin(AValue: Integer); virtual;
      procedure DoGesture(const EventInfo: TGestureEventInfo; var Handled: Boolean); override;
      procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
      procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
      procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
      procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
      procedure DoMouseLeave; override;
      procedure DblClick; override;
    public
      procedure AbortLoading;
      procedure WaitForFinish;
      property ImageURL: string read GetImageURL write SetImageURL;
      property ImageManager: IImageWithUrlManager read GetImageManager write SetImageManager;
      property IsLoadingNow: boolean read GetIsLoadingNow;
      property BitmapIWU: TBitmap read GetBitmapIWU;
      property OnLoadingFinished: TOnLoadingFinishedEvent read GetOnLoadingFinished write SetOnLoadingFinished;
    { ❤ --------------- }
      procedure ZoomPicture(AZoomValue: integer);
      procedure FitWidth;
      procedure FitHeight;
      property MoveFactor: single read FMoveFactor write FMoveFactor;
      property ZoomFactor: single read FZoomFactor write FZoomFactor;
      property InvertMouseWheel: boolean read FInvertMouseWheel write FInvertMouseWheel;
      property EnableZoomingOutOfFit: boolean read FEnableZoomingOutOfFit write FEnableZoomingOutOfFit;
      property BestFitOnDoubleTap: boolean read FBestFitOnDoubleTap write FBestFitOnDoubleTap;
      property OnZoomBegin: TZoomingEvent read FOnZoomBegin write FOnZoomBegin;
      Constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
  End;

implementation


constructor TImageWithUrlViewer.Create(AOwner: TComponent);
begin
  inherited;
  FLock := TMREWSync.Create;
  FImageURL := '';
  FImageManager := nil;

  {$IFNDEF ANDROID}
    FZoomFactor := 0.001;
  {$ELSE}
    FZoomFactor := 0.005;
  {$ENDIF}

  FMoveFactor := 1.85;
  FEnableZoomingOutOfFit := False;
  FBestFitOnDoubleTap    := True;

  Self.AniCalculations.TouchTracking := [];
  Self.ShowBackground := False;
  Self.MouseScaling := False;

  Self.Touch.InteractiveGestures := [
    TInteractiveGesture.Pan,
    TInteractiveGesture.Zoom,
    TInteractiveGesture.DoubleTap
  ];
end;

procedure TImageWithUrlViewer.DblClick;
begin
  inherited;
  if BestFitOnDoubleTap then
    Self.BestFit;
end;

destructor TImageWithUrlViewer.Destroy;
begin
  AbortLoading;
  WaitForFinish;
  FLock.Free;
  inherited;
end;

function TImageWithUrlViewer.GetBitmapIWU: TBitmap;
begin
  Result := Self.Bitmap;
end;

function TImageWithUrlViewer.GetImageManager: IImageWithUrlManager;
begin
  Result := FImageManager;
end;

function TImageWithUrlViewer.GetImageURL: string;
begin
  FLock.BeginRead;
  try
    Result := FImageURL;
  finally
    FLock.EndRead;
  end;
end;

function TImageWithUrlViewer.GetIsLoadingNow: boolean;
begin
  if Assigned(ImageManager) then
    Result := ImageManager.IsLoadingNow(self);
end;

function TImageWithUrlViewer.GetOnLoadingFinished: TOnLoadingFinishedEvent;
begin
  Result := FOnLoadingFinished;
end;

procedure TImageWithUrlViewer.AbortLoading;
begin
  if Assigned(ImageManager) then
    ImageManager.AbortImage(Self);
end;

procedure TImageWithUrlViewer.SetImageManager(const Value: IImageWithUrlManager);
begin
  if Assigned(ImageManager) then
    AbortLoading;

  FImageManager := value;
end;

procedure TImageWithUrlViewer.SetImageURL(const Value: string);
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

procedure TImageWithUrlViewer.SetOnLoadingFinished(const Value: TOnLoadingFinishedEvent);
begin
  FOnLoadingFinished := Value;
end;

procedure TImageWithUrlViewer.WaitForFinish;
begin
  IWUWaitForFinish(Self);
end;

procedure TImageWithUrlViewer.ZoomPicture(AZoomValue: integer);
begin
  Self.DoZoom(AZoomValue);
end;

procedure TImageWithUrlViewer.DoGesture(const EventInfo: TGestureEventInfo;
  var Handled: Boolean);
begin
  if EventInfo.GestureID = igiZoom then
  begin
    begin
      FinishMove;

      if (not (TInteractiveGestureFlag.gfBegin in EventInfo.Flags) )
      and (not (TInteractiveGestureFlag.gfEnd in EventInfo.Flags) ) then
        Self.DoZoom(EventInfo.Distance - FLastZoomDist);

      FLastZoomDist := EventInfo.Distance;
    end;
  end else if EventInfo.GestureID = igiDoubleTap then begin
    Self.DblClick;
  end;
  inherited;
end;


procedure TImageWithUrlViewer.DoZoom(AValue: Integer);
var
  LNewPos: TPointF;
  LOldViewportPosition: TPointF;
  LViewRect: TRectF;
  LZoomPlusValue: single;

  procedure _process(var AVal: single; APrevSize, ANewSize, AViewCenter, AViewSize: single);
  var
    LPercentInPrev: single;
    LPercentsInVal: single;
    LPercent: single;
  begin
    LPercentInPrev := APrevSize / 100;
    LPercentsInVal := (AViewCenter / LPercentInPrev);
    LPercent := ANewSize / 100;

    AVal := (LPercentsInVal * LPercent);
    AVal := AVal - AViewSize / 2;
  end;

begin
  Self.DoOnZoomBegin(AValue);
  Self.BeginUpdate;
  try
    FPrevContentBounds := Self.ContentBounds.Size;
    LOldViewportPosition := Self.ViewportPosition;

    LViewRect := TRectF.Create(
      LOldViewPortPosition.X,
      LOldViewPortPosition.Y,
      Self.Width + LOldViewPortPosition.X,
      Self.Height + LOldViewPortPosition.Y
    );

    LZoomPlusValue := (AValue * ZoomFactor);
    Self.BitmapScale := Self.BitmapScale + LZoomPlusValue;

    LNewPos := LOldViewportPosition;
    var LContentBounds := Self.DoCalcContentBounds;

    if (not EnableZoomingOutOfFit)
    and (LContentBounds.Height < Self.Height)
    and (LContentBounds.Width < Self.Width) then begin
      Self.BestFit;
    end else begin

      _process(LNewPos.X, FPrevContentBounds.Width, LContentBounds.Width,
         LViewRect.CenterPoint.X, LViewRect.Width);

      _process(LNewPos.Y, FPrevContentBounds.Height, LContentBounds.Height,
         LViewRect.CenterPoint.Y, LViewRect.Height);

      Self.ViewportPosition := LNewPos;
    end;
  finally
    Self.EndUpdate;
  end;
end;

procedure TImageWithUrlViewer.FinishMove;
begin
  Self.FNeedToMove := False;
end;

procedure TImageWithUrlViewer.FitHeight;
begin
  FitWithSize(0, Bitmap.Height);
end;

procedure TImageWithUrlViewer.FitWidth;
begin
  FitWithSize(Bitmap.Width, 0);
end;

procedure TImageWithUrlViewer.FitWithSize(AWidth, AHeight: integer);
var
  R: TRectF;
  s: Single;
begin
  { modified TImageViewer.BestFit; }
  if (Content <> nil) and (ContentLayout <> nil) then
  begin
    R := RectF(0, 0, AWidth, AHeight);
    s := R.Fit(ContentLayout.LocalRect);
    if s >= 1 then
      BitmapScale := 1 / s
    else
      BitmapScale := 1;
    if (VScrollBar <> nil) and (VScrollBar.Enabled)
    or (HScrollBar <> nil) and (HScrollBar.Enabled) then
    begin { Need realign with enabled scrolls }
      R := RectF(0, 0, AWidth, AHeight);
      s := R.Fit(ContentLayout.LocalRect);
      if s >= 1 then
        BitmapScale := 1 / s
      else
        BitmapScale := 1;
    end;
  end
end;

procedure TImageWithUrlViewer.DoMouseLeave;
begin
  inherited;
  FNeedToMove := False;
end;

procedure TImageWithUrlViewer.DoOnZoomBegin(AValue: Integer);
begin
  if Assigned(FOnZoomBegin) then
    FOnZoomBegin(AValue)
end;

procedure TImageWithUrlViewer.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
begin
  inherited;
  FMovePoint := TPointF.Create(X, Y);
  FNeedToMove := True;
end;

procedure TImageWithUrlViewer.MouseMove(Shift: TShiftState; X, Y: Single);
var
  LNewPoint: TPointF;

  procedure _process(APrev: single; var AVal: single);
  begin
    if AVal > APrev then
      AVal := +(AVal - APrev)
    else if (AVal < APrev) then
      AVal := -(APrev - AVal)
    else
      AVal := 0;
  end;

begin

  LNewPoint := TPointF.Create(X, Y);
  if FNeedToMove then begin

    _process(FMovePoint.X, X);
    _process(FMovePoint.Y, Y);

    X := X * FMoveFactor;
    Y := Y * FMoveFactor;

    Self.ScrollTo(X, Y);
  end;
  FMovePoint := LNewPoint;

  inherited;
end;

procedure TImageWithUrlViewer.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;
  FinishMove;
end;

procedure TImageWithUrlViewer.MouseWheel(Shift: TShiftState; WheelDelta: Integer;
  var Handled: Boolean);
begin
  Handled := True;
  if FInvertMouseWheel then WheelDelta := -WheelDelta;
  Self.DoZoom(WheelDelta);
  inherited;
end;

end.
