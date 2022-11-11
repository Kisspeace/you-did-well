{ ❤ 2022 by Kisspeace - https://github.com/Kisspeace }
{ ❤ Part of you-did-well library ❤ ---------------- }
unit YDW.FMX.ImageWithURLManager.Base;

interface
uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Objects, YDW.FMX.ImageWithURL.Interfaces, YDW.Threading;

type

  TMultiThreadImageContentManagerAbs = Class(TComponent, IImageWithURLManager)
    protected type
      TWorkerThread = Class(TGenericYDWQueuedThreadInterface<IImageWithUrl>)
        protected
          FOwner: TMultiThreadImageContentManagerAbs;
          procedure SubThreadExecute(AValue: IImageWithURL); override;
      End;
    protected
      FWorker: TWorkerThread;
      FLock: TMREWSync;
      FCacheManager: IImageWithUrlCacheManager;
      FEnableSaveToCache: boolean;
      FEnableLoadFromCache: boolean;
      function GetCacheManager: IImageWithUrlCacheManager; virtual;
      procedure SetCacheManager(const Value: IImageWithUrlCacheManager); virtual;
      function GetEnableSaveToCache: boolean;
      procedure SetEnableSaveToCache(const Value: boolean);
      function GetEnableLoadFromCache: boolean;
      procedure SetEnableLoadFromCache(const Value: boolean);
      function GetThreadsCount: integer;
      procedure SetThreadsCount(const Value: integer);
      { ❤-- override this on child class -- }
      procedure SubThreadExecute(AValue: IImageWithURL); virtual; abstract;
    public
      procedure LoadImage(AImage: IImageWithUrl); virtual;
      procedure AbortImage(AImage: IImageWithUrl); virtual;
      function IsLoadingNow(AImage: IImageWithUrl): boolean;
      function WaitForItem(AImage: IImageWithUrl): LongWord;
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
    published
      property CacheManager: IImageWithUrlCacheManager read GetCacheManager write SetCacheManager; { this poroperty is thread unsafe }
      property EnableSaveToCache: boolean read GetEnableSaveToCache write SetEnableSaveToCache default True;
      property EnableLoadFromCache: boolean read GetEnableLoadFromCache write SetEnableLoadFromCache default True;
      property ThreadsCount: integer read GetThreadsCount write SetThreadsCount;
  End;

implementation

{ TImageContentManagerAbs }

procedure TMultiThreadImageContentManagerAbs.AbortImage(AImage: IImageWithUrl);
begin
  FWorker.AbortItem(AImage);
end;

constructor TMultiThreadImageContentManagerAbs.Create;
begin
  inherited;
  FWorker := TWorkerThread.Create;
  FWorker.FOwner := Self;
  FLock := FWorker.FLock;
  FCacheManager := Nil;
  FEnableSaveToCache := True;
  FEnableLoadFromCache := True;
end;


destructor TMultiThreadImageContentManagerAbs.Destroy;
begin
  Self.FWorker.Terminate;
  Self.FWorker.WaitFor;
  FWorker.Free;
  inherited;
end;

function TMultiThreadImageContentManagerAbs.GetCacheManager: IImageWithUrlCacheManager;
begin
  Result := FCacheManager;
end;

function TMultiThreadImageContentManagerAbs.GetEnableLoadFromCache: boolean;
begin
  FLock.BeginRead;
  Result := FEnableLoadFromCache;
  FLock.EndRead;
end;

function TMultiThreadImageContentManagerAbs.GetEnableSaveToCache: boolean;
begin
  FLock.BeginRead;
  Result := FEnableSaveToCache;
  FLock.EndRead;
end;

function TMultiThreadImageContentManagerAbs.GetThreadsCount: integer;
begin
  Result := FWorker.ThreadsCount;
end;

function TMultiThreadImageContentManagerAbs.IsLoadingNow(
  AImage: IImageWithUrl): boolean;
var
  I: integer;
begin
  Result := False;

  FWorker.FLock.BeginRead;
  try

    // Searching image in Running list
    Result := ( FWorker.RunningIndex(AImage) <> -1 );
    if Result then exit;

    // Searching image in queue list
    Result := ( FWorker.FQueue.IndexOf(AImage) <> -1 );

  finally
    FWorker.FLock.EndRead;
  end;
end;

procedure TMultiThreadImageContentManagerAbs.LoadImage(AImage: IImageWithUrl);
begin
  FWorker.QueueAdd(AImage);
end;


procedure TMultiThreadImageContentManagerAbs.SetCacheManager(
  const Value: IImageWithUrlCacheManager);
begin
  FCacheManager := Value;
end;


procedure TMultiThreadImageContentManagerAbs.SetEnableLoadFromCache(
  const Value: boolean);
begin
  FLock.BeginWrite;
  FEnableLoadFromCache := Value;
  FLock.EndWrite;
end;

procedure TMultiThreadImageContentManagerAbs.SetEnableSaveToCache(
  const Value: boolean);
begin
  FLock.BeginWrite;
  FEnableSaveToCache := Value;
  FLock.EndWrite;
end;

procedure TMultiThreadImageContentManagerAbs.SetThreadsCount(
  const Value: integer);
begin
  FWorker.ThreadsCount := Value;
end;

function TMultiThreadImageContentManagerAbs.WaitForItem(
  AImage: IImageWithUrl): LongWord;
begin
  Result := Self.FWorker.WaitForItem(AImage);
end;

{ TMultiThreadImageContentManagerAbs.TWorkerThread }

procedure TMultiThreadImageContentManagerAbs.TWorkerThread.SubThreadExecute(
  AValue: IImageWithURL);
begin
  FOwner.SubThreadExecute(AValue);
end;

end.
