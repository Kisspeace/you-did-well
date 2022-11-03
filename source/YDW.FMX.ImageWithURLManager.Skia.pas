{ ❤ 2022 by Kisspeace - https://github.com/Kisspeace }
{ ❤ Part of you-did-well library ❤ ---------------- }
unit YDW.FMX.ImageWithURLManager.Skia;

interface
uses
  System.Classes, System.SysUtils, YDW.FMX.ImageWithURLManager,
  YDW.FMX.ImageWithURL.Interfaces, skia;

type

  TImageWithUrlManagerSkia = Class(TImageWithUrlManager, IImageWithURLManager)
    private
      function GetEncodedImageQuality: integer;
      function GetEncodeImageFormat: TSkEncodedImageFormat;
      procedure SetEncodedImageQuality(const Value: integer);
      procedure SetEncodeImageFormat(const Value: TSkEncodedImageFormat);
    protected
      FEncodedImageQuality: integer;
      FEncodeImageFormat: TSkEncodedImageFormat;
      procedure EncodeImage(AStream: TStream; Out AEncodedImage: TStream); override;
    public
      constructor Create(AOwner: TComponent); override;
    published
      property EncodeImageFormat: TSkEncodedImageFormat read GetEncodeImageFormat write SetEncodeImageFormat default TSkEncodedImageFormat.JPEG;
      property EncodedImageQuality: integer read GetEncodedImageQuality write SetEncodedImageQuality default 80;
  End;

implementation

{ TImageWithUrlManagerSkia }

constructor TImageWithUrlManagerSkia.Create(AOwner: TComponent);
begin
  inherited;
  FEncodedImageQuality := 80;
  FEncodeImageFormat := TSkEncodedImageFormat.JPEG;
end;

procedure TImageWithUrlManagerSkia.EncodeImage(AStream: TStream;
  out AEncodedImage: TStream);
const
  WEBP_MAGICK: TArray<Byte> = [$52, $49, $46, $46]; // RIFF
//  JPEG_MAGICK: TArray<Byte> = [$FF, $D8, $FF, $E0];
var
  LImage: ISkImage;
  LMagick: TArray<Byte>;
begin
  AStream.Position := 0;
  SetLength(LMagick, 4);
  AStream.ReadBuffer(Pointer(LMagick)^, length(LMagick));
  AStream.Position := 0;

  if CompareMem(WEBP_MAGICK, LMagick, Length(LMagick)) then begin
    LImage := TSkImage.MakeFromEncodedStream(AStream);
    AEncodedImage := TMemoryStream.Create;
    LImage.EncodeToStream(AEncodedImage, EncodeImageFormat, EncodedImageQuality);
    AEncodedImage.Position := 0;
  end else begin
    inherited;
  end;
end;

function TImageWithUrlManagerSkia.GetEncodedImageQuality: integer;
begin
  FLock.BeginRead;
  Result := Self.FEncodedImageQuality;
  FLock.EndRead;
end;

function TImageWithUrlManagerSkia.GetEncodeImageFormat: TSkEncodedImageFormat;
begin
  FLock.BeginRead;
  Result := Self.FEncodeImageFormat;
  FLock.EndRead;
end;

procedure TImageWithUrlManagerSkia.SetEncodedImageQuality(const Value: integer);
begin
  FLock.BeginWrite;
  Self.FEncodedImageQuality := Value;
  FLock.EndWrite;
end;

procedure TImageWithUrlManagerSkia.SetEncodeImageFormat(
  const Value: TSkEncodedImageFormat);
begin
  FLock.BeginWrite;
  Self.FEncodeImageFormat := Value;
  FLock.EndWrite;
end;

end.
