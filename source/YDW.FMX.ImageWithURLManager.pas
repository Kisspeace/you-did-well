{ ❤ 2022 by Kisspeace - https://github.com/Kisspeace }
{ ❤ Part of you-did-well library ❤ ---------------- }
unit YDW.FMX.ImageWithURLManager;

interface
uses
  {$IFDEF YDW_DEBUG}
  YDW.Debug,
  {$ENDIF}
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Objects, YDW.FMX.ImageWithURL.Interfaces, YDW.Threading,
  YDW.FMX.ImageWithURLManager.Base, System.Net.HttpClient,
  System.Net.HttpClientComponent, System.Net.URLClient,
  System.IOUtils, FMX.Graphics;

type

  TNetHttpClientEvent = reference to procedure (Sender: TObject; AClient: TNetHttpClient);
  TIWULoadExceptionEvent = reference to procedure (Sender: TObject; AImage: IImageWithURL; const AUrl: string; const AException: Exception);
  TFilterResponseEvent = reference to procedure (Sender: TObject; const AUrl: string; const AResponse: IHttpResponse; var AAllow: boolean);

  TImageWithUrlManager = Class(TMultiThreadImageContentManagerAbs, IImageWithURLManager)
    protected const
      { ❤ - default headers }
      DEF_USERAGENT = 'Mozilla/5.0 (Windows NT 10.0; rv:102.0) Gecko/20100101 Firefox/102.0';
      {$IFDEF MSWINDOWS}
      DEF_ACCEPT_ENCODING = 'gzip, deflate';
      {$ELSE}
      DEF_ACCEPT_ENCODING = '';
      {$ENDIF}
      DEF_WEB_TIMEOUTS = 6000;
    private
      function GetLoadThumbnailFromFile: boolean;
      procedure SetLoadThumbnailFromFile(const Value: boolean);
      function GetThumbSize: TSizeF;
      procedure SetThumbSize(const Value: TSizeF);
      function GetSyncBitmapLoadFromFile: boolean;
      procedure SetSyncBitmapLoadFromFile(const Value: boolean);
    protected
      FLoadThumbnailFromFile: boolean;
      FThumbSize: TSizeF;
      FOnWebClientCreate: TNetHttpClientEvent;
      FOnImageLoadException: TIWULoadExceptionEvent;
      FOnFilterResponse: TFilterResponseEvent;
      FSyncBitmapLoadFromFile: boolean;
      procedure EncodeImage(AStream: TStream; Out AEncodedImage: TStream); virtual;
      procedure SubThreadExecute(AValue: IImageWithURL); override;
    public
      constructor Create(AOwner: Tcomponent); override;
    published
      property LoadThumbnailFromFile: boolean read GetLoadThumbnailFromFile write SetLoadThumbnailFromFile default True;
      property ThumbSize: TSizeF read GetThumbSize write SetThumbSize;
      property OnWebClientCreate: TNetHttpClientEvent read FOnWebClientCreate write FOnWebClientCreate;
      property OnFilterResponse: TFilterResponseEvent read FOnFilterResponse write FOnFilterResponse;
      property OnImageLoadException: TIWULoadExceptionEvent read FOnImageLoadException write FOnImageLoadException;
      property SyncBitmapLoadFromFile: boolean read GetSyncBitmapLoadFromFile write SetSyncBitmapLoadFromFile default True;
  End;

implementation

{ TImageWithUrlManager }

constructor TImageWithUrlManager.Create(AOwner: Tcomponent);
begin
  inherited;
  FThumbSize := TSizeF.Create(512, 512);
  FLoadThumbnailFromFile := True;
  FSyncBitmapLoadFromFile := True;
end;

procedure TImageWithUrlManager.EncodeImage(AStream: TStream;
  out AEncodedImage: TStream);
begin
  AEncodedImage := AStream;
end;

function TImageWithUrlManager.GetLoadThumbnailFromFile: boolean;
begin
  FLock.BeginRead;
  Result := self.FLoadThumbnailFromFile;
  FLock.EndRead;
end;

function TImageWithUrlManager.GetSyncBitmapLoadFromFile: boolean;
begin
  FLock.BeginRead;
  Result := FSyncBitmapLoadFromFile;
  FLock.EndRead;
end;

function TImageWithUrlManager.GetThumbSize: TSizeF;
begin
  FLock.BeginRead;
  Result := FThumbSize;
  FLock.EndRead;
end;

procedure TImageWithUrlManager.SetLoadThumbnailFromFile(const Value: boolean);
begin
  FLock.BeginWrite;
  FLoadThumbnailFromFile := Value;
  FLock.EndWrite;
end;

procedure TImageWithUrlManager.SetSyncBitmapLoadFromFile(const Value: boolean);
begin
  FLock.BeginWrite;
  FSyncBitmapLoadFromFile := Value;
  FLock.EndWrite;
end;

procedure TImageWithUrlManager.SetThumbSize(const Value: TSizeF);
begin
  FLock.BeginWrite;
  FThumbSize := Value;
  FLock.EndWrite;
end;

procedure TImageWithUrlManager.SubThreadExecute(AValue: IImageWithURL);
var
  LImage: IImageWithURL;
  LClient: TNetHttpClient;
  LResponse: IHttpResponse;
  LUrl: string;
  LImageLoaded: boolean;
  LFinalImage: TStream;
  LBufBmp: TBitmap;

  function IsValidHTTPURL(const AURL: string): boolean;
  begin
    Result := AURL.StartsWith(TURI.SCHEME_HTTPS, True);
    if not Result then
      Result := AURL.StartsWith(TURI.SCHEME_HTTP, True);
  end;

  function IsStoredLocal(const AURL: string): boolean;
  begin
    Result := not IsValidHTTPURL(AURL);
    if Result then
      Result := FileExists(AURL);
  end;

  procedure DoOnException(AExcept: Exception);
  begin
    {$IFDEF YDW_DEBUG}
    if not (AExcept is ENetException) then
      Log('DoOnException', AExcept);
    {$ENDIF}
    LImageLoaded := False;
    if Assigned(FOnImageLoadException) then
      FOnImageLoadException(Self, LImage, LUrl, AExcept);
  end;

  function AllowThisResponse(const AUrl: string; const AResponse: IHttpResponse): boolean;
  var
    LAllow: boolean;
  begin
    LAllow := TRUE;
    if Assigned(OnFilterResponse) then
      OnFilterResponse(Self, AUrl, AResponse, LAllow);
    Result := LAllow;
  end;

begin
  LImage := AValue;
  LUrl := LImage.ImageURL; // Saved local url
  LFinalImage := nil;

  try
    try
      if EnableLoadFromCache
      and Assigned(CacheManager)
      and CacheManager.IsCached(LUrl) then begin

        // Have copy on cache
        CacheManager.LoadFromCache(LUrl, LImage.BitmapIWU);
        LImageLoaded := True;

      end else if IsStoredLocal(LUrl) then begin

        // Url is file path and file exists
        if SyncBitmapLoadFromFile then begin

          TThread.Synchronize(TThread.Current, procedure
          begin
            if Self.LoadThumbnailFromFile then
              LImage.BitmapIWU.LoadThumbnailFromFile(LUrl, ThumbSize.Width, ThumbSize.Height) // ISSUE (FormOnCreate, FormOnShow)
            else
              LImage.BitmapIWU.LoadFromFile(LUrl);  // ISSUE (FormOnCreate, FormOnShow)
            LImageLoaded := True;
          end);

        end else begin // Usign buffer bitmap

          LBufBmp := TBitmap.Create;
          try
            {$IFDEF YDW_DEBUG} try {$ENDIF}
            if Self.LoadThumbnailFromFile then
              LBufBmp.LoadThumbnailFromFile(LUrl, ThumbSize.Width, ThumbSize.Height) // ISSUE (FormOnCreate, FormOnShow)
            else
              LBufBmp.LoadFromFile(LUrl);  // ISSUE (FormOnCreate, FormOnShow)

            TThread.Synchronize(TThread.Current,
            procedure
            begin
              LImage.BitmapIWU.Assign(LBufBmp);
              LImageLoaded := True;
            end);

            {$IFDEF YDW_DEBUG}
            Except
              on E: Exception do begin
                Log('IsStoredLocal async load', E);
                raise;
              end;
            end;
            {$ENDIF}

          finally
            LBufBmp.Free;
          end;

        end;

      end else begin
        // Trying to load from HTTP/HTTPS Url
        try
          LClient := TNetHttpClient.Create(nil);

          if Assigned(OnWebClientCreate) then
            OnWebClientCreate(Self, LClient)
          else begin
            LClient.UserAgent := DEF_USERAGENT;
            LClient.AcceptEncoding := DEF_ACCEPT_ENCODING;
            LClient.ResponseTimeout := DEF_WEB_TIMEOUTS;
            LClient.SendTimeout := DEF_WEB_TIMEOUTS;
            LClient.ConnectionTimeout := DEF_WEB_TIMEOUTS;
          end;

          LClient.SynchronizeEvents := False;
          LClient.Asynchronous := False;
          LClient.AutomaticDecompression := [THTTPCompressionMethod.Any];

          if TThread.Current.CheckTerminated then exit;
          LResponse := LClient.Get(LUrl);
          if TThread.Current.CheckTerminated then exit;

          if not AllowThisResponse(LUrl, LResponse) then exit;

          EncodeImage(LResponse.ContentStream, LFinalImage);

          if EnableSaveToCache and Assigned(CacheManager) then begin
            if not CacheManager.IsCached(LUrl) then
              CacheManager.CacheItem(LUrl, LFinalImage);
          end;

          TThread.synchronize(TThread.Current,
          procedure
          begin
            LImage.BitmapIWU.LoadFromStream(LFinalImage);
            LImageLoaded := True;
          end);

        finally

          if Assigned(LResponse)
          and (LFinalImage <> LResponse.ContentStream) then
            FreeAndNil(LFinalImage);

          LClient.Free;

        end;

      end;

    except
      On E: Exception do
        DoOnException(E);
    end;

  finally

    if Assigned(LImage.OnLoadingFinished) then begin
      TThread.Synchronize(TThread.Current,
      procedure
      begin
        LImage.OnLoadingFinished(LImage as TObject, LImageLoaded);
      end);
    end;

  end;
end;

end.
