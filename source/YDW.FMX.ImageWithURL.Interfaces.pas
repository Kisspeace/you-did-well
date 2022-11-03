{ ❤ 2022 by Kisspeace - https://github.com/Kisspeace }
{ ❤ Part of you-did-well library ❤ ---------------- }
unit YDW.FMX.ImageWithURL.Interfaces;

interface
uses
  System.SysUtils, System.Types, Classes,
  FMX.Graphics { - TBitmap here } ;

type

  TOnLoadingFinishedEvent = reference to procedure (Sender: TObject; ASuccess: boolean);

  IImageWithUrlManager = Interface;
  IImageWithUrlCacheManager = Interface;

  IImageWithUrl = Interface { IWU }
    ['{1B23E6F8-AED8-4356-913C-E09A3E404BCD}']
    { ❤ -- protected --- }
    function GetImageURL: string;
    procedure SetImageURL(const Value: string);
    function GetImageManager: IImageWithUrlManager;
    procedure SetImageManager(const Value: IImageWithUrlManager);
    function GetIsLoadingNow: boolean;
    function GetBitmapIWU: TBitmap;
    function GetOnLoadingFinished: TOnLoadingFinishedEvent;
    procedure SetOnLoadingFinished(const Value: TOnLoadingFinishedEvent);
    { ❤ -- public ------ }
    procedure AbortLoading;
    procedure WaitForFinish;
    { ❤ -- properties -- }
    property ImageURL: string read GetImageURL write SetImageURL;
    property ImageManager: IImageWithUrlManager read GetImageManager write SetImageManager;
    property IsLoadingNow: boolean read GetIsLoadingNow;
    property BitmapIWU: TBitmap read GetBitmapIWU; // ugly name to prevent overloading "Bitmap" property on TImage
    property OnLoadingFinished: TOnLoadingFinishedEvent read GetOnLoadingFinished write SetOnLoadingFinished;
  End;

  IImageWithUrlManager = Interface
    ['{95763454-E8A2-4B60-A2B8-A2C5AB37ABF6}']
    { ❤ -- protected --- }
    function GetCacheManager: IImageWithUrlCacheManager;
    procedure SetCacheManager(const Value: IImageWithUrlCacheManager);
    function GetEnableSaveToCache: boolean;
    procedure SetEnableSaveToCache(const Value: boolean);
    function GetEnableLoadFromCache: boolean;
    procedure SetEnableLoadFromCache(const Value: boolean);
    { ❤ -- public ------ }
    procedure LoadImage(AImage: IImageWithUrl);
    procedure AbortImage(AImage: IImageWithUrl);
    function IsLoadingNow(AImage: IImageWithUrl): boolean;
    { ❤ -- properties -- }
    property CacheManager: IImageWithUrlCacheManager read GetCacheManager write SetCacheManager;
    property EnableSaveToCache: boolean read GetEnableSaveToCache write SetEnableSaveToCache;
    property EnableLoadFromCache: boolean read GetEnableLoadFromCache write SetEnableLoadFromCache;
  End;

  IImageWithUrlCacheManager = Interface
    ['{306FF6AC-21A6-4C59-A4A2-6209C649234B}']
    { ❤ -- public ------ }
    procedure CacheItem(AUrl: string; ASource: TStream); // Save image and Url in cache
    procedure LoadFromCache(AUrl: string; ABitmap: TBitmap);
    function IsCached(AUrl: string): boolean;
  End;

  procedure IWUWaitForFinish(AImage: IImageWithURL);

implementation

procedure IWUWaitForFinish(AImage: IImageWithURL);
const
  WAIT_TIMEOUT = 10;
begin
  while AImage.IsLoadingNow do begin
    if ( TThread.Current.ThreadID = MainThreadId ) then begin
      CheckSynchronize(WAIT_TIMEOUT);
    end else
      Sleep(WAIT_TIMEOUT);
  end;
end;

end.
