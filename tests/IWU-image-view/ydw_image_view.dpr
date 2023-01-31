program ydw_image_view;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  YDW.FMX.ImageWithURL.ImageViewer in '..\..\source\YDW.FMX.ImageWithURL.ImageViewer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
