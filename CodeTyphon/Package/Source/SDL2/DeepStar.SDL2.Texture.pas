unit DeepStar.SDL2.Texture;

{$mode ObjFPC}{$H+}
{$ModeSwitch unicodestrings}{$J-}
{$ModeSwitch advancedrecords}

interface

uses
  Classes,
  SysUtils,
  LazUTF8,
  System.UITypes,
  DeepStar.Utils,
  libSDL2,
  libSDL2_image,
  libSDL2_ttf;

type
  PTexture = ^TTexture;
  TTexture = object(TObj)
  private type
    float = single;
    TColors = System.UITypes.TAlphaColorRec;

  public type
    TScale = record
    public
      x, y: single;
      class function Create: TScale; static;
      class operator initialize(var obj: TScale);
    end;

  private
    _height: integer;
    _width: integer;
    _data: PSDL_Texture;
    _position: TPoint;
    _scale: TScale;

    function __GetBoundsRect: TRect;
    function __GetData: PSDL_Texture;
    function __GetHeight: integer;
    function __GetPosition: TPoint;
    function __GetWidth: integer;

    procedure __Free;

  public
    constructor Init;
    destructor Done; virtual;

    class function Create: TTexture; static;
    class function CreatePtr: PTexture; static;

    function GetScale: TTexture.TScale;

    // 指定路径图像创建纹理
    procedure LoadFromFile(renderer: PSDL_Renderer; path: string);

    // 从字符串创建纹理
    procedure LoadFormString(renderer: PSDL_Renderer; ttfName: string; ttfSize: integer;
      Text: string; color: TSDL_Color);

    procedure SetPosition(ax, ay: integer);
    procedure SetPosition(ap: TPoint);
    procedure SetColor(color: TColors);
    procedure SetScale(x, y: float);

    property Width: integer read __GetWidth;
    property Height: integer read __GetHeight;
    property Data: PSDL_Texture read __GetData;
    property Position: TPoint read __GetPosition;
    property BoundsRect: TRect read __GetBoundsRect;
  end;

implementation

  { TTexture }

constructor TTexture.Init();
begin
  _scale := TScale.Create;
end;

class function TTexture.Create: TTexture;
begin
  Result.Init;
end;

class function TTexture.CreatePtr: PTexture;
begin
  New(Result, Init);
end;

destructor TTexture.Done;
begin
  Self.__Free;

  inherited;
end;

function TTexture.GetScale: TTexture.TScale;
begin
  Result.x := _scale.x;
  Result.y := _scale.y;
end;

procedure TTexture.LoadFormString(renderer: PSDL_Renderer; ttfName: string;
  ttfSize: integer; Text: string; color: TSDL_Color);
var
  textSurface: PSDL_Surface;
  errStr: string;
  font: PTTF_Font;
  newTexture: PSDL_Texture;
begin
  Self.__Free;

  font := PTTF_Font(nil);
  font := TTF_OpenFont(CrossFixFileName(ttfName).ToPAnsiChar, ttfSize);
  if font = nil then
  begin
    errStr := 'Failed to load font! SDL_ttf Error: %s';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;

  textSurface := PSDL_Surface(nil);
  try
    textSurface := TTF_RenderUTF8_Blended(font, Text.ToPAnsiChar, color);

    if textSurface = nil then
    begin
      errStr := 'Unable to render text surface! SDL_ttf Error: %s';
      errStr.Format([SDL_GetError()]);
      raise Exception.Create(errStr.ToAnsiString);
    end;

    // Create texture from surface pixels
    newTexture := PSDL_Texture(nil);
    newTexture := SDL_CreateTextureFromSurface(renderer, textSurface);
    if newTexture = nil then
    begin
      errStr := 'Unable to create texture from rendered text! SDL Error: %s';
      errStr.Format([SDL_GetError()]);
      raise Exception.Create(errStr.ToAnsiString);
    end;

    // Get image dimensions
    _width := textSurface^.w;
    _height := textSurface^.h;
    _data := newTexture;
  finally
    TTF_CloseFont(font);
    SDL_FreeSurface(textSurface);
  end;
end;

procedure TTexture.LoadFromFile(renderer: PSDL_Renderer; path: string);
var
  fileName, errStr: string;
  loadedSurface: PSDL_Surface;
  newTexture: PSDL_Texture;
begin
  __Free;

  fileName := CrossFixFileName(path);
  // Load image at specified path
  loadedSurface := PSDL_Surface(nil);
  loadedSurface := IMG_Load(fileName.ToPAnsiChar);
  if loadedSurface = nil then
  begin
    errStr := 'Unable to load image %s! SDL_image Error.';
    errStr.Format([fileName]);
    raise Exception.Create(errStr.ToAnsiString);
  end
  else
  begin
    // The final texture
    // Create texture from surface pixels
    try
      newTexture := PSDL_Texture(nil);
      newTexture := SDL_CreateTextureFromSurface(renderer, loadedSurface);
      if newTexture = nil then
      begin
        errStr := 'Unable to create texture from %s! SDL Error: %s';
        errStr.Format([fileName, SDL_GetError()]);
        raise Exception.Create(errStr.ToAnsiString);
      end;

      _width := loadedSurface^.w;
      _height := loadedSurface^.h;
      _data := newTexture;
    finally
      // Get rid of old loaded surface
      SDL_FreeSurface(loadedSurface);
    end;
  end;
end;

procedure TTexture.SetColor(color: TColors);
begin
  SDL_SetTextureColorMod(_data, color.R, color.G, color.B);
end;

procedure TTexture.SetPosition(ax, ay: integer);
begin
  _position := TPoint.Create(ax, ay);
end;

procedure TTexture.SetPosition(ap: TPoint);
begin
  _position := ap;
end;

procedure TTexture.SetScale(x, y: float);
begin
  _scale.x := x;
  _scale.y := y;
end;

procedure TTexture.__Free;
begin
  if _data <> nil then
  begin
    SDL_DestroyTexture(_data);
    _data := nil;

    _height := 0;
    _width := 0;
  end;
end;

function TTexture.__GetBoundsRect: TRect;
begin
  Result := Rect(
    _position.x,
    _position.y,
    _position.x + _width,
    _position.y + _height);
end;

function TTexture.__GetData: PSDL_Texture;
begin
  Result := _data;
end;

function TTexture.__GetHeight: integer;
begin
  Result := _height;
end;

function TTexture.__GetPosition: TPoint;
begin
  Result := _position;
end;

function TTexture.__GetWidth: integer;
begin
  Result := _width;
end;

{ TTexture.TScale }

class operator TTexture.TScale.initialize(var obj: TScale);
begin
  obj.x := 1;
  obj.y := 1;
end;

class function TTexture.TScale.Create: TScale;
begin
  initialize(Result);
end;

end.
