unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  YDW.FMX.ImageWithURL.Interfaces, YDW.FMX.ImageWithURLCacheManager,
  YDW.FMX.ImageWithURLManager, FMX.Layouts, FMX.ExtCtrls,
  YDW.FMX.ImageWithURL.ImageViewer, FMX.Objects, FMX.Edit,
  FMX.Controls.Presentation, FMX.StdCtrls, System.IOUtils;

type
  TForm1 = class(TForm)
    MainLayer: TLayout;
    MainMenu: TVertScrollBox;
    Button1: TButton;
    EditZoom: TEdit;
    EditMove: TEdit;
    Text1: TText;
    Text2: TText;
    EditURL: TEdit;
    ClearEditButton1: TClearEditButton;
    Button2: TButton;
    CheckOutOfFit: TCheckBox;
    CheckDoubleTap: TCheckBox;
    Rectangle1: TRectangle;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ChangeUI(AControl: TControl);
  end;

var
  ImageViewer: TImageWithUrlViewer;
  ImgManager: TImageWithUrlManager;
  ImgCacheManager: TImageWithURLCahceManager;
  Form1: TForm1;

  DefMove: Single;
  DefZoom: Single;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
  ImageViewer.ZoomFactor := Form1.EditZoom.Text.ToSingle;
  ImageViewer.MoveFactor := Form1.EditMove.Text.ToSingle;
  ImageViewer.BestFitOnDoubleTap := Form1.CheckDoubleTap.IsChecked;
  ImageViewer.EnableZoomingOutOfFit := Form1.CheckOutOfFit.IsChecked;
  ImageViewer.ImageURL := Form1.EditURL.Text;
  ChangeUI(ImageViewer);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Form1.EditZoom.Text := DefZoom.ToString;
  Form1.EditMove.Text := DefMove.ToString;
  Form1.EditURL.Text := 'https://wallpaperaccess.com/full/673011.jpg';
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Form1.FullScreen := (not Form1.FullScreen);
end;

procedure TForm1.ChangeUI(AControl: TControl);
var
  I: integer;
begin
  for I := 0 to MainLayer.Controls.Count - 1 do
    MainLayer.Controls[I].Visible := False;
  AControl.Visible := True;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ImgCacheManager := TImageWithURLCahceManager.Create(self);

  {$IFDEF MSWINDOWS}
  ImgCacheManager.SetSaveAndLoadPath(TPath.Combine(ExtractFilePath(ParamStr(0)), 'cache\images\'));
  {$ENDIF}

  {$IFDEF ANDROID}
  ImgCacheManager.SetSaveAndLoadPath(Tpath.Combine(Tpath.GetCachePath, 'images'));
  {$ENDIF}

  ImgManager := TImageWithUrlManager.Create(self);
  ImgManager.CacheManager := ImgCacheManager;

  ImageViewer := TImageWithUrlViewer.Create(Self.MainLayer);
  ImageViewer.Parent := Self.MainLayer;
  ImageViewer.Align := TAlignLayout.Client;
  ImageViewer.ImageManager := ImgManager;
  ImageViewer.ShowBackground := True;
  ImageViewer.BackgroundFill := Form1.Fill;

  DefMove := ImageViewer.MoveFactor;
  DefZoom := ImageViewer.ZoomFactor;

  form1.EditZoom.Text := DefZoom.ToString;
  Form1.EditMove.Text := DefMove.ToString;
  Form1.CheckOutOfFit.IsChecked := ImageViewer.EnableZoomingOutOfFit;
  Form1.CheckDoubleTap.IsChecked := ImageViewer.BestFitOnDoubleTap;

  ChangeUI(MainMenu);

  {$IFDEF ANDROID}
  Form1.EditZoom.StyledSettings := [];
  Form1.EditMove.StyledSettings := [];
  Form1.EditURL.StyledSettings := [];
  Form1.EditZoom.TextSettings.FontColor := TAlphaColorRec.White;
  Form1.EditMove.TextSettings.FontColor := TAlphaColorRec.White;
  Form1.EditUrl.TextSettings.FontColor := TAlphaColorRec.White;
  {$ENDIF}
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  if (key = VkHardwareBack) or (key = VkEscape) then
    ChangeUI(MainMenu);
  key := 0;
end;

end.
