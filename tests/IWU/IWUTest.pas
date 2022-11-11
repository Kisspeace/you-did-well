unit IWUTest;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl,
  FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  FMX.StdCtrls, FMX.Layouts, FMX.Objects, YDW.FMX.ImageWithURL,
  System.Generics.Collections, System.Net.URLClient, System.Net.HttpClient,
  System.Net.HttpClientComponent, System.IOUtils, System.Threading,
  YDW.FMX.ImageWithURLManager, YDW.FMX.ImageWithURL.Interfaces, FMX.Edit,
  DateUtils, YDW.FMX.ImageWithURLCacheManager, YDW.FMX.ImageWithURL.Module,
  FMX.ExtCtrls, YDW.Threading, Ydw.Debug;

type

  TIMageScrollBrowser = Class(TVertScrollBox)
    protected type
      TWorker = Class(TGenericYDWQueuedThreadObject<TStrings>)
        protected
          Browser: TIMageScrollBrowser;
          procedure SubThreadExecute(AItem: TStrings); override;
      End;
    protected
      FWorker: TWorker;
    public
      Images: TObjectList<TImageWithUrl>;
      procedure Go(AUrls: TArray<String>);
      procedure Clear;
      constructor Create(AOwner: TComponent); override;
      Destructor Destroy; override;
  end;

  TForm2 = class(TForm)
    Tabs: TTabControl;
    TabItem1: TTabItem;
    GroupBox1: TGroupBox;
    actions: TVertScrollBox;
    Button1: TButton;
    Display: TText;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button6: TButton;
    Button7: TButton;
    Layout1: TLayout;
    EditThreads: TEdit;
    Text2: TText;
    CheckCacheSave: TCheckBox;
    CheckCacheLoad: TCheckBox;
    VertScrollBox1: TVertScrollBox;
    GridPanelLayout1: TGridPanelLayout;
    TabItem3: TTabItem;
    MemoUrls: TMemo;
    TabItem4: TTabItem;
    Button8: TButton;
    IWUModuleTestLayout: TLayout;
    Circle1: TCircle;
    Circle2: TCircle;
    Circle3: TCircle;
    Circle4: TCircle;
    RoundRect1: TRoundRect;
    Image1: TImage;
    CalloutRectangle1: TCalloutRectangle;
    Pie1: TPie;
    ImageControl1: TImageControl;
    Button9: TButton;
    LogTab: TTabItem;
    LogMemo: TMemo;
    Stress: TTabItem;
    BtnStressStart: TButton;
    StressEdit: TEdit;
    StressLayout: TLayout;
    procedure FormCreate(Sender: TObject);
    procedure EditThreadsChange(Sender: TObject);
    procedure CheckCacheLoadChange(Sender: TObject);
    procedure CheckCacheSaveChange(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure BtnStressStartClick(Sender: TObject);
    procedure DisplayClick(Sender: TObject);
  private
    procedure OnLoadingExceptAutoRestart(Sender: TObject; AImage: IImageWithURL;
      const AUrl: string; const AException: Exception);
    { Private declarations }
  public
    { Public declarations }
    procedure OnImageLoaded(Sender: TObject; ASuccess: boolean);
    procedure OnLoadingExcept(Sender: TObject; AImage: IImageWithURL; const AUrl: string; const AException: Exception);
    function AddImage: TImageWithUrl;
    function SetupIWU(AOwner: TComponent): TIWUModule;
    function RandUrl: string;
    function GetFinishedCount: integer;
    procedure SetImgageManager(AManager: IImageWithUrlManager);
    function GetThreadsCount: integer;
    procedure DisplayInfo;
    procedure ClearBitmap(ABitmap: TBitmap);

    procedure RecreateManager;
    procedure AbortAll;
    procedure SetRandUrls;
    procedure ClearBitmaps;
    procedure StartAndAbortHard;
  end;

  TUrlImages = TObjectList<TImageWithUrl>;

var
  Form2: TForm2;
  Images: TUrlImages;
  ImgManager: TImageWithUrlManager;
  ImgCacheManager: TImageWithURLCahceManager;
  StressTest: TIMageScrollBrowser;

const
  LOG_FILENAME: string = 'log.txt';
  GRID_COLUMNS: integer = 10;
  GRID_ROWS: integer = 10;

//  procedure Log(AStr: string);
//  procedure SyncLog(AStr: string);

implementation

{$R *.fmx}

procedure WriteToFile(AFileName, Atext: string);
var
  F: TFileStream;
  Encoding: TEncoding;
  LBytes: TBytes;
  LDir: string;
begin
  try
    LDir := Tpath.GetDirectoryName(AFilename);
    if Not DirectoryExists(LDir) then
      CreateDir(LDir);

    if FileExists(AFilename) then
      F := TFileStream.Create(AFilename, FmOpenWrite)
    else
      F := TFileStream.Create(AFilename, FmCreate);

    Encoding := TEncoding.Default;
    LBytes := Encoding.GetBytes(AText);
    F.Position := F.Size;
    F.Write(LBytes, 0, Length(LBytes));
  finally
    F.Free;
  end;
end;

//procedure Log(AStr: string);
//var
//  LText: string;
//begin
//  LText := '[' + Now.ToString + ']: ' + AStr;
//  WriteToFile(LOG_FILENAME, LText + SLineBreak);
//  Form2.LogMemo.Lines.Add(LText);
//end;
//
//procedure SyncLog(AStr: string);
//begin
//  TThread.Synchronize(nil, procedure begin Log(AStr); end);
//end;

function TForm2.GetFinishedCount: integer;
var
  I: integer;
begin
  Result := 0;
  for I := 0 to Images.Count - 1 do begin
    if not Images[I].IsLoadingNow then
      Inc(Result);
  end;
end;

procedure TForm2.AbortAll;
var
  I: integer;
begin
  for I := 0 to Images.Count - 1 do begin
    Images[I].AbortLoading;
  end;
end;

function TForm2.AddImage: TImageWithUrl;
begin
  Result := TImageWithUrl.Create(Self);
  with Result do begin
    Parent := Form2.GridPanelLayout1;
    Align := TAlignLayout.Client;
    ImageManager := ImgManager;
    OnLoadingFinished := Form2.OnImageLoaded;
  end;
  Images.Add(Result);
end;

procedure TForm2.BtnStressStartClick(Sender: TObject);
var
  LCount: integer;
  LWorker: TThread;

begin
  LCount := StressEdit.Text.ToInteger;

  LWorker := TThread.CreateAnonymousThread(
  procedure
  var
    I: integer;

    function GetUrls(ACount: integer = 20): TArray<String>;
    var
      I: integer;
    begin
      Result := [];
      for I := 1 to ACount do begin
        Result := Result + [Self.RandUrl];
      end;
    end;

  begin
    for I := 1 to LCount do begin
      Form2.Caption := I.ToString + ' \ ' + LCount.ToString;
      StressTest.Go(GetUrls(15));
      sleep(Random(10));

      TThread.Synchronize(LWorker, procedure begin
        Application.ProcessMessages;
      end);
      
      if I mod 3 = 0 then begin
        StressTest.Clear;
      end;


      TThread.Synchronize(LWorker, procedure begin
        Application.ProcessMessages;
        CheckSynchronize(10);
      end);

    end;
  end);

  LWorker.FreeOnTerminate := False;
  LWorker.Start;
  LWorker.WaitFor;

  StressTest.Clear;
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  Self.SetRandUrls;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  Self.AbortAll;
end;

procedure TForm2.Button3Click(Sender: TObject);
begin
  Self.SetRandUrls;
  Self.AbortAll;
end;

procedure TForm2.Button4Click(Sender: TObject);
begin
  Self.AbortAll;
  Self.SetRandUrls;
end;

procedure TForm2.Button6Click(Sender: TObject);
begin
  ClearBitmaps;
end;

procedure TForm2.Button7Click(Sender: TObject);
begin
  Self.RecreateManager;
end;

procedure TForm2.Button8Click(Sender: TObject);
var
  I: integer;
begin
  for I := 0 to Self.IWUModuleTestLayout.Controls.Count - 1 do begin
    with IWUModule(IWUModuleTestLayout.Controls[I]) do begin
      ImageUrl := RandUrl;
    end;
  end;
end;

procedure TForm2.Button9Click(Sender: TObject);
begin
  Self.StartAndAbortHard;
end;

procedure TForm2.CheckCacheLoadChange(Sender: TObject);
begin
  ImgManager.EnableLoadFromCache := (Sender as TCheckBox).IsChecked;
end;

procedure TForm2.CheckCacheSaveChange(Sender: TObject);
begin
  ImgManager.EnableSaveToCache := (Sender as TCheckBox).IsChecked;
end;

procedure TForm2.ClearBitmap(ABitmap: TBitmap);
begin
  ABitmap.SetSize(1, 1);
  ABitmap.Clear(TAlphaColorRec.Black);
end;

procedure TForm2.ClearBitmaps;
var
  I: integer;
begin
  for I := 0 to Images.Count - 1 do begin
    Images[I].BitmapIWU.Resize(0, 0);
  end;
end;

procedure TForm2.DisplayClick(Sender: TObject);
begin
  Self.DisplayInfo;
end;

procedure TForm2.DisplayInfo;
var
  Finished: integer;
  LText: string;
begin
  Finished := Self.GetFinishedCount;
  if ( finished < Images.Count) then begin
    LText := 'Finished: ' + finished.ToString + ' / ' + Images.Count.ToString;
    LText := LText + SLineBreak + 'Push to refresh';
  end else
    LText := 'Complete!';


  Display.Text := LText;
end;

procedure TForm2.EditThreadsChange(Sender: TObject);
begin
  ImgManager.ThreadsCount := GetThreadsCount;
end;

procedure TForm2.FormCreate(Sender: TObject);
var
  Count, I: integer;
  Percents: single;
begin
  ImgCacheManager := TImageWithURLCahceManager.Create(self);

  {$IFDEF MSWINDOWS}
  ImgCacheManager.SetSaveAndLoadPath(TPath.Combine(ExtractFilePath(ParamStr(0)), 'cache\images\'));
  {$ENDIF}

  {$IFDEF ANDROID}
  ImgCacheManager.SetSaveAndLoadPath(Tpath.Combine(Tpath.GetCachePath, 'images'));
  {$ENDIF}


  Images := TUrlImages.Create;
  ImgManager := nil;
  RecreateManager;

  Form2.GridPanelLayout1.ColumnCollection.Clear;
  Form2.GridPanelLayout1.RowCollection.Clear;

  for I := 1 to GRID_COLUMNS do
    Form2.GridPanelLayout1.ColumnCollection.Add;

  for I := 1 to GRID_ROWS do
    Form2.GridPanelLayout1.RowCollection.Add;

  Count := form2.GridPanelLayout1.ColumnCollection.Count;
  Percents := trunc(100 / Count);
  for I := 0 to Count - 1 do begin
    Form2.GridPanelLayout1.ColumnCollection.Items[I].Value := Percents;
  end;

  Count := form2.GridPanelLayout1.RowCollection.Count;
  Percents := trunc(100 / Count);
  for I := 0 to Count - 1 do begin
    Form2.GridPanelLayout1.RowCollection.Items[I].Value := Percents;
  end;

  for I := 1 to Form2.GridPanelLayout1.CellCount do
    Form2.AddImage;

  for I := 0 to Self.IWUModuleTestLayout.Controls.Count - 1 do begin
    Self.SetupIWU(IWUModuleTestLayout.Controls[I]);
  end;

  StressTest := TImageScrollBrowser.Create(Self);
  StressTest.Parent := Form2.StressLayout;
  StressTest.Align := TAlignLayout.Client;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  ImgManager.Free;
end;

function TForm2.GetThreadsCount: integer;
var
  Value: integer;
begin
  try
    Result := EditThreads.Text.ToInteger;
  Except
    Result := 2;
  end;
end;

procedure TForm2.OnImageLoaded(Sender: TObject; ASuccess: boolean);
begin
  if not ASuccess then begin
    var LImage: IImageWithURL;
    if Supports(Sender, IImageWithURL, LImage) then 
      ClearBitmap(LImage.BitmapIWU);
  end;
  DisplayInfo;
end;

procedure TForm2.OnLoadingExcept(Sender: TObject; AImage: IImageWithURL;
  const AUrl: string; const AException: Exception);
begin
  TThread.Synchronize(nil, procedure begin
    AImage.BitmapIWU.Clear(TAlphaColorrec.Black);
  end);
end;

procedure TForm2.OnLoadingExceptAutoRestart(Sender: TObject;
  AImage: IImageWithURL; const AUrl: string; const AException: Exception);
begin
  AImage.ImageURL := AUrl;
end;

function TForm2.RandUrl: string;
var
  Index: integer;
begin
  Index := Random(Form2.MemoUrls.Lines.Count);
  Result := Form2.MemoUrls.Lines.Strings[Index];
end;

procedure TForm2.RecreateManager;
begin
  if Assigned(ImgManager) then
    ImgManager.Free;

  ImgManager := TImageWithUrlManager.Create(self);
  ImgManager.ThreadsCount := GetThreadsCount;
  ImgManager.CacheManager := ImgCacheManager;
  ImgManager.EnableSaveToCache := CheckCacheSave.IsChecked;
  ImgManager.EnableLoadFromCache := CheckCacheLoad.IsChecked;
  ImgManager.LoadThumbnailFromFile := True;
  ImgManager.ThumbSize := TSizeF.Create(512, 512);
  ImgManager.OnImageLoadException := OnLoadingExcept;
  Self.SetImgageManager(ImgManager);
end;

procedure TForm2.SetImgageManager(AManager: IImageWithUrlManager);
var
  I: integer;
begin
  for I := 0 to Images.Count - 1 do begin
    Images[I].ImageManager := AManager;
  end;

  for I := 0 to Self.IWUModuleTestLayout.Controls.Count - 1 do begin
    var LModule := IWUModule(IWUModuleTestLayout.Controls[I]);
    if Assigned(LModule) then begin
      LModule.ImageManager := ImgManager;
    end;
  end;
end;

procedure TForm2.SetRandUrls;
var
  I: integer;
begin
  for I := 0 to Images.Count - 1 do begin
    Images[I].ImageURL := RandUrl;
  end;
end;

function TForm2.SetupIWU(AOwner: TComponent): TIWUModule;
begin
  Result := TIWUModule.Create(AOwner);
  Result.ImageManager := ImgManager;
end;

procedure TForm2.StartAndAbortHard;
var
  I: integer;
begin
  for I := 1 to 40 do begin
    Self.SetRandUrls;
    Self.AbortAll;
    Sleep(Random(40));
    CheckSynchronize(10);
  end;
end;


{ TIMageScrollBrowser }

procedure TIMageScrollBrowser.Clear;
var
  I: integer;

  function GetWorkCount: integer;
  var
    I: integer;
  begin
    Result := 0;
    for I := 0 to Images.Count - 1 do begin
      if Images[I].IsLoadingNow then
        Inc(Result);
    end;
  end;

begin
  FWorker.Terminate;
  FWorker.WaitFor;
  Images.Clear;
end;

constructor TIMageScrollBrowser.Create(AOwner: TComponent);
begin
  inherited;
  FWorker := TWorker.Create;
  FWorker.Browser := Self;
  Self.FWorker.ThreadsCount := 1;
  Images := TObjectList<TImageWithUrl>.Create;
end;

destructor TIMageScrollBrowser.Destroy;
begin
  Self.Clear;
  FWorker.Free;
  Images.Free;
  inherited;
end;

procedure TIMageScrollBrowser.Go(AUrls: TArray<String>);
var
  Lstrs : TStrings;
begin
  LStrs := TStringList.Create;
  LStrs.AddStrings(Aurls);
  Fworker.QueueAdd(LStrs);
end;

{ TIMageScrollBrowser.TWorker }

procedure TIMageScrollBrowser.TWorker.SubThreadExecute(AItem: TStrings);
var
  I: Integer;
  ID: cardinal;
begin
  try
    ID := TTHread.Current.ThreadID;

    Sleep(Random(200));
    if TThread.Current.CheckTerminated then exit;
    Sleep(Random(30));

    TThread.Synchronize(nil, procedure
    var
      I: integer;
    begin
      For I := 0 to AItem.Count - 1 do begin
        try
          var LImage: TImageWithUrl;
          LImage := TImageWithUrl.Create(self.Browser);
          LImage.Height := 50 + Random(130);
          LImage.Parent := Self.Browser;
          Form2.ClearBitmap(Limage.BitmapIWU);
          LImage.Align := TAlignLayout.Top;
          LImage.Position.Y := Single.MaxValue; // most bottom
          LImage.Padding.Rect := TRectF.Create(2, 2, 2, 2);
          LImage.Tag := RANDOM(NativeInt.MaxValue);
          LImage.ImageManager := ImgManager;
          Browser.Images.Add(LImage);

          LImage.ImageURL := AItem.Strings[I];
        except on E: Exception do

        end;
      end;
    end);

    Sleep(Random(12));
  finally
    AItem.Free;
  end;
end;

end.
