unit Unit2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl,
  FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  FMX.StdCtrls, FMX.Layouts, FMX.Objects, YDW.FMX.ImageWithURL,
  System.Generics.Collections, System.Net.URLClient, System.Net.HttpClient,
  System.Net.HttpClientComponent, System.IOUtils, System.Threading,
  YDW.FMX.ImageWithURLManager.Skia, YDW.FMX.ImageWithURL.Interfaces, FMX.Edit,
  DateUtils, YDW.FMX.ImageWithURLCacheManager, YDW.FMX.ImageWithURL.Module,
  FMX.ExtCtrls;

type

  TForm2 = class(TForm)
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem3: TTabItem;
    MemoUrls: TMemo;
    GroupBox1: TGroupBox;
    actions: TVertScrollBox;
    Button1: TButton;
    VertScrollBox1: TVertScrollBox;
    GridPanelLayout1: TGridPanelLayout;
    Display: TText;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Layout1: TLayout;
    Edit1: TEdit;
    Text2: TText;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    TabItem4: TTabItem;
    RoundRect1: TRoundRect;
    Circle1: TCircle;
    Circle3: TCircle;
    Circle4: TCircle;
    Button8: TButton;
    Circle2: TCircle;
    IWUModuleTestLayout: TLayout;
    Image1: TImage;
    CalloutRectangle1: TCalloutRectangle;
    Pie1: TPie;
    ImageControl1: TImageControl;
    TabItem2: TTabItem;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure Button8Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure OnImageLoaded(Sender: TObject; ASuccess: boolean);
    procedure OnImageLoadException(Sender: TObject; AImage: IImageWithURL; const AUrl: string; const AException: Exception);
    function AddImage: TImageWithUrl;
    function SetupIWU(AOwner: TComponent): TIWUModule;
    function RandUrl: string;
    function GetFinishedCount: integer;
    procedure SetImgageManager(AManager: IImageWithUrlManager);
    function GetThreadsCount: integer;
    procedure DisplayInfo;
  end;

  TUrlImages = TObjectList<TImageWithUrl>;

var
  Form2: TForm2;
  Images: TUrlImages;
  ImgManager: TImageWithUrlManagerSkia;
  ImgCacheManager: TImageWithURLCahceManager;
  SuccessCount: integer;

const
  GRID_COLUMNS: integer = 10;
  GRID_ROWS: integer = 10;

  procedure Log(AStr: string);

implementation

{$R *.fmx}

procedure Log(AStr: string);
begin
  if TThread.Current.ThreadID = MainThreadId then
    Form2.Memo1.Lines.Add('[' + DateTimeToStr(Now) + ']: ' + AStr)
  else
    TThread.Synchronize(nil, procedure begin Log(AStr) end);
end;

function TForm2.AddImage: TImageWithUrl;
begin
  Result := TImageWithUrl.Create(Self);
  Result.Parent := Form2.GridPanelLayout1;
  Result.Align := TAlignLayout.Client;
  Result.ImageManager := ImgManager;
  Result.OnLoadingFinished := Form2.OnImageLoaded;
  Images.Add(Result);
end;

procedure TForm2.Button1Click(Sender: TObject);
var
  I: integer;
begin
  for I := 0 to Images.Count - 1 do begin
    Images[I].ImageURL := RandUrl;
  end;
end;

procedure TForm2.Button2Click(Sender: TObject);
var
  I: integer;
begin
  for I := 0 to Images.Count - 1 do begin
    Images[I].AbortLoading;
  end;
end;

procedure TForm2.Button3Click(Sender: TObject);
begin
  Button1.OnClick(nil);
  Button2.OnClick(nil);
end;

procedure TForm2.Button4Click(Sender: TObject);
begin
  Button2.OnClick(nil);
  Button1.OnClick(nil);
end;

procedure TForm2.Button5Click(Sender: TObject);
var
  I: integer;
begin
  for I := 0 to 2 do begin
    Button4.OnClick(nil);
    Sleep(Random(100));
  end;
  Button1.OnClick(Nil);
end;

procedure TForm2.Button6Click(Sender: TObject);
var
  I: integer;
begin
  for I := 0 to Images.Count - 1 do begin
    Images[I].BitmapIWU.Resize(0, 0);
  end;
end;

procedure TForm2.Button7Click(Sender: TObject);
begin
  ImgManager.Free;
  ImgManager := TImageWithUrlManagerSkia.Create(self);
  ImgManager.ThreadsCount := GetThreadsCount;
  ImgManager.CacheManager := ImgCacheManager;
  ImgManager.EnableSaveToCache := CheckBox1.IsChecked;
  ImgManager.EnableLoadFromCache := CheckBox2.IsChecked;
  Self.SetImgageManager(ImgManager);
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

procedure TForm2.CheckBox1Change(Sender: TObject);
begin
  ImgManager.EnableSaveToCache := (Sender as TCheckBox).IsChecked;
end;

procedure TForm2.CheckBox2Change(Sender: TObject);
begin
  ImgManager.EnableLoadFromCache := (Sender as TCheckBox).IsChecked;
end;

procedure TForm2.DisplayInfo;
var
  Finished: integer;
  LText: string;
begin
  Finished := Self.GetFinishedCount;
  if ( finished < Images.Count) then
    LText := 'Loading: ' + finished.ToString + ' / ' + Images.Count.ToString
  else
    LText := 'Loading: Complete!';

  Display.Text := LText;
end;

procedure TForm2.Edit1Change(Sender: TObject);
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
  ImgCacheManager.SetSaveAndLoadPath(Tpath.Combine(Tpath.GetCachePath, 'cache\images\'));
  {$ENDIF}

  ImgManager := TImageWithUrlManagerSkia.Create(Self);
  ImgManager.ThreadsCount := GetThreadsCount;
  ImgManager.CacheManager := ImgCacheManager;
  ImgManager.ThumbSize := TSizeF.Create(512, 512);
  ImgManager.LoadThumbnailFromFile := True;
  ImgManager.OnImageLoadException := Self.OnImageLoadException;

  Images := TUrlImages.Create;

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

  for I := 1 to Form2.GridPanelLayout1.CellCount do begin
    Var LControl: TControl;
    LControl := Form2.AddImage;
    LControl.Tag := I;
  end;

  for I := 0 to Self.IWUModuleTestLayout.Controls.Count - 1 do begin
    var LImage: IImageWithUrl;
    LImage := Self.SetupIWU(IWUModuleTestLayout.Controls[I]);
    LImage.OnLoadingFinished := Self.OnImageLoaded;
  end;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  ImgManager.Free;
end;

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

function TForm2.GetThreadsCount: integer;
var
  Value: integer;
begin
  try
    Result := Form2.Edit1.Text.ToInteger;
  Except
    Result := 2;
  end;
end;

procedure TForm2.OnImageLoaded(Sender: TObject; ASuccess: boolean);
begin
  DisplayInfo;
end;

procedure TForm2.OnImageLoadException(Sender: TObject; AImage: IImageWithURL;
  const AUrl: string; const AException: Exception);
begin
  log((AImage as TControl).Tag.ToString + ': ' + AException.ToString);
  TThread.Synchronize(nil,
  procedure begin
    AImage.BitmapIWU.SetSize(1, 1);
    AImage.BitmapIWU.Clear(TAlphaColorrec.Black);
  end);
end;

function TForm2.RandUrl: string;
var
  Index: integer;
begin
  Index := Random(Form2.MemoUrls.Lines.Count);
  Result := Form2.MemoUrls.Lines.Strings[Index];
end;

procedure TForm2.SetImgageManager(AManager: IImageWithUrlManager);
var
  I: integer;
begin
  for I := 0 to Images.Count - 1 do begin
    Images[I].ImageManager := AManager;
  end;
end;

function TForm2.SetupIWU(AOwner: TComponent): TIWUModule;
begin
  Result := TIWUModule.Create(AOwner);
  Result.ImageManager := ImgManager;
end;

end.
