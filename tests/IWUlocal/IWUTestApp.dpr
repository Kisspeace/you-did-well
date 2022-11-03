program IWUTestApp;

uses
  System.StartUpCopy,
  FMX.Forms,
  IWUTest in 'IWUTest.pas' {Form2},
  YDW.FMX.ImageWithURL.Interfaces in '..\..\source\YDW.FMX.ImageWithURL.Interfaces.pas',
  YDW.FMX.ImageWithURL.Module in '..\..\source\YDW.FMX.ImageWithURL.Module.pas',
  YDW.FMX.ImageWithURL in '..\..\source\YDW.FMX.ImageWithURL.pas',
  YDW.FMX.ImageWithURL.Rectangle in '..\..\source\YDW.FMX.ImageWithURL.Rectangle.pas',
  YDW.FMX.ImageWithURLCacheManager in '..\..\source\YDW.FMX.ImageWithURLCacheManager.pas',
  YDW.FMX.ImageWithURLManager.Base in '..\..\source\YDW.FMX.ImageWithURLManager.Base.pas',
  YDW.FMX.ImageWithURLManager in '..\..\source\YDW.FMX.ImageWithURLManager.pas',
  YDW.Threading in '..\..\source\YDW.Threading.pas',
  YDW.Debug in '..\..\source\debug\YDW.Debug.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
