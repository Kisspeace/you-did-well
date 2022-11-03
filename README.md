[![written_on](https://img.shields.io/badge/_-RAD_Studio-darkcyan?style=for-the-badge&logo=delphi)](https://www.embarcadero.com/products/rad-studio/)
![Platforms](https://img.shields.io/badge/Android-1A2541?style=for-the-badge&logo=android&logoColor=white)
![Platforms](https://img.shields.io/badge/Windows-1A2541?style=for-the-badge&logo=windows)
![RepoSize](https://img.shields.io/github/repo-size/Kisspeace/you-did-well?style=for-the-badge)

## 🖼 Image with URL 🔗
You want to display many images from internet in your FMX application ?  
This library make it easy !

### 🔰 With TImage:
```pascal
uses 
    YDW.FMX.ImageWithURL.pas, // TImageWithURL here
    YDW.FMX.ImageWithURLManager.pas, // You can use YDW.FMX.ImageWithURLManager.Skia.pas if you want .webp support
    YDW.FMX.ImageWithURLCacheManager.pas; // Cache manager that store all images in a hard drive
```
```pascal
var CacheManager: TImageWithURLCahceManager := TImageWithURLCahceManager.Create; 
CacheManager.SetSaveAndLoadPath('cache\images\'); // Path where manager store content (directory creates automatically)

ImageManager: TImageWithURLManager := TImageWithURLManager.Create;
ImageManager.ThreadsCount := 8; // Max threads count that executes at the same time
ImageManager.CacheManager := CacheManager; // can be nil

var Image: TImageWithURL := TImageWithURL.Create(Form1);
Image.Parent := Form1;
Image.ImageManager := ImageManager;
Image.ImageURL := 'https://media.tenor.com/G1x0D7j67OgAAAAi/trust-in-my-healing-sage.gif'; { And now just set URL }
Image.ImageURL := 'C:\Windows\Web\Wallpaper\Windows\img0.jpg'; { or local file path }
```

### 🔰 With TIWUModule:
```pascal
uses 
    YDW.FMX.ImageWithURL.Module.pas, // TIWUModule here
    YDW.FMX.ImageWithURLManager.pas, // You can use YDW.FMX.ImageWithURLManager.Skia.pas if you want .webp support
    YDW.FMX.ImageWithURLCacheManager.pas; // Cache manager that store all images in a hard drive
```
```pascal
var Module: TIWUModule;
Module := TIWUModule.Create(Image1); // (TImage, TShape(TRectangle, TCircle ..))
Module := TIWUModule.Create(Image1, Image1.Bitmap); // Or set target bitmap manualy

{ Usage type #1: }
Module.ImageURL := ''; // set Url or Local file name here

{ Usage type #2: }
With IWUModule(Image1) do begin
    { Image1 is a component that have TIWUModule object in their components list }
    ImageURL := ''; // set Url or Local file name here
end;

```

## ⚠️ Known bugs:
* 🧯 Set ImageURL property on FormCreate / FormShow can throw Access violation when (ImageWithURLManager.SyncBitmapLoadFromFile = FALSE) or (CacheManager.SyncBitmapLoadFromFile = FALSE)