program ImageWithURLSkiaTest;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit2 in 'Unit2.pas' {Form2},
  YDW.FMX.ImageWithURL.Interfaces in '..\..\source\YDW.FMX.ImageWithURL.Interfaces.pas',
  YDW.FMX.ImageWithURL in '..\..\source\YDW.FMX.ImageWithURL.pas',
  YDW.FMX.ImageWithURLManager in '..\..\source\YDW.FMX.ImageWithURLManager.pas',
  YDW.Threading in '..\..\source\YDW.Threading.pas',
  YDW.FMX.ImageWithURLManager.Base in '..\..\source\YDW.FMX.ImageWithURLManager.Base.pas',
  YDW.FMX.ImageWithURLCacheManager in '..\..\source\YDW.FMX.ImageWithURLCacheManager.pas',
  YDW.FMX.ImageWithURL.Rectangle in '..\..\source\YDW.FMX.ImageWithURL.Rectangle.pas',
  YDW.FMX.ImageWithURL.AlRectangle in '..\..\source\YDW.FMX.ImageWithURL.AlRectangle.pas',
  YDW.FMX.ImageWithURL.Module in '..\..\source\YDW.FMX.ImageWithURL.Module.pas',
  YDW.FMX.ImageWithURLManager.Skia in '..\..\source\YDW.FMX.ImageWithURLManager.Skia.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
