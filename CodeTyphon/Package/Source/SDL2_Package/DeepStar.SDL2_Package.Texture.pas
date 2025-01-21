unit DeepStar.SDL2_Package.Texture;

{$mode ObjFPC}{$H+}
{$ModeSwitch unicodestrings}{$J-}
{$ModeSwitch advancedrecords}

interface

uses
  Classes,
  SysUtils,
  LazUTF8,
  libSDL2,
  libSDL2_image,
  libSDL2_ttf,
  libSDL2_gfx,
  DeepStar.Utils;

type
  TCustomImage = class (TInterfacedObject)
  protected class var
    _ImageRefCount: integer;

  private
    procedure __IMG_Init;
    procedure __TTF_Init;

  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TTexture = Class(TCustomImage)
  public type
    TScale = record
    public
      x, y: single;
      class function Create: TScale; static;
      class operator initialize(var obj: TScale);
    end;

  private
    _Height: integer;
    _Renderer: PSDL_Renderer;
    _Width: integer;
    _Texture: PSDL_Texture;
    _Position: TPoint;
    _Scale: TScale;
    _Color: TSDL_Color;

    function __GetBoundsRect: TRect;
    function __GetData: PSDL_Texture;
    function __GetHeight: integer;
    function __GetPosition: TPoint;
    function __GetWidth: integer;

    procedure __GetRGBA(color: TSDL_Color; out r, g, b, a: Byte);

    procedure __Free;

  public
    constructor Create; override;
    constructor Create(renderer: PSDL_Renderer);
    destructor Destroy; override;

    // 新建一个空白 纹理
    function CreateBlank(width, Height: integer): Boolean;

    function GetScale: TTexture.TScale;
    function ToPSDL_Texture: PSDL_Texture;

    // 指定路径图像创建纹理
    procedure LoadFromFile(path: string);

    // 从字符串创建纹理
    procedure LoadFormString(ttfName: string; ttfSize: integer; Text: string;
      color: TSDL_Color);

    // 将当前纹理设置为渲染目标
    procedure SetTarget;
    // 取消当前纹理设置为渲染目标
    procedure UnsetTarget;

    // 渲染纹理
    procedure Render(srcRect: PSDL_Rect = nil; destRect: PSDL_Rect = nil);
    // 在给定点渲染纹理
    procedure Render(p: TPoint);
    procedure Render(x, y: integer; clip: PSDL_Rect = nil; angle: double = 0;
      center: PSDL_Point = nil; flip: TSDL_RendererFlags = SDL_FLIP_NONE);

    procedure SetRenderer(renderer: PSDL_Renderer);

    //(*═══════════════════════════════════════════════════════════════════════
    // 绘图函数

    procedure Clear;

    procedure SetDrawColor;
    procedure SetDrawColor(color: TSDL_Color);
    procedure SetDrawColor(r, g, b, a: Byte);


    procedure DrawCircle(x, y, rad: integer);
    procedure DrawCircleA(x, y, rad: integer);
    procedure DrawCircleAndFilled(x, y, rad: integer);

    //═══════════════════════════════════════════════════════════════════════*)

    procedure SetPosition(ax, ay: integer);
    procedure SetPosition(ap: TPoint);
    procedure SetColorMod(color: TSDL_Color);
    procedure SetScale(x, y: float);

    property Width: integer read __GetWidth;
    property Height: integer read __GetHeight;
    property Position: TPoint read __GetPosition;
    property BoundsRect: TRect read __GetBoundsRect;
  end;

implementation

uses
  DeepStar.SDL2_Package.Utils;

{ TCustomImage }

constructor TCustomImage.Create;
begin
  if _ImageRefCount = 0 then
  begin
    __IMG_Init;
    __TTF_Init;
  end;

  _ImageRefCount += 1;
end;

destructor TCustomImage.Destroy;
begin
  _ImageRefCount -= 1;

  if _ImageRefCount <= 0 then
  begin
    IMG_Quit;
    TTF_Quit;
  end;

  inherited Destroy;
end;

procedure TCustomImage.__IMG_Init;
var
  errStr: string;
  flagJpg, flagPng: integer;
begin
  flagPng := IMG_Init(IMG_INIT_PNG);
  flagJpg := IMG_Init(IMG_INIT_JPG);

  if (flagJpg <= 0) and (flagPng <= 0) then
  begin
    errStr := 'SDL_image could not initialize! SDL_image Error: %s';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;
end;

procedure TCustomImage.__TTF_Init;
var
  errStr: string;
begin
  if TTF_Init() = -1 then
  begin
    errStr := 'SDL_ttf could not initialize! SDL_ttf Error: %s';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;
end;

  { TTexture }

constructor TTexture.Create(renderer: PSDL_Renderer);
begin
  Create;

  _Scale := TScale.Create;
  _Color := TSDL_Color(TAlphaColors.White);

  Self.SetRenderer(renderer);
end;

constructor TTexture.Create;
begin
  inherited Create;
end;

procedure TTexture.Clear;
var
  err: integer;
  errStr: String;
begin
  err := SDL_RenderClear(_Renderer);
  if err <> 0 then
  begin
    errStr := 'Failed to call the SDL_RenderClear()! SDL Error: %s';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;
end;

function TTexture.CreateBlank(width, Height: integer): Boolean;
var
  errString: String;
begin
  __Free;

  _Texture := SDL_CreateTexture
  (
    _Renderer,
    SDL_PIXELFORMAT_RGBA8888,
    SDL_TEXTUREACCESS_TARGET,
    Width,
    Height
  );

  if _Texture = nil then
  begin
    errString := Format
    (
      'Unable to create streamable blank texture! SDL Error: %s',
      [SDL_GetError()]
    ).ToUString;
    raise Exception.Create(errString.ToAnsiString);
  end
  else
  begin
    _Width := Width;
    _Height := Height;
  end;

  Result := _Texture <> nil;
end;

destructor TTexture.Destroy;
begin
  __Free;
  inherited Destroy;
end;

procedure TTexture.DrawCircle(x, y, rad: integer);
var
  r, g, b, a: Byte;
  err: Integer;
  errStr: String;
begin
  __GetRGBA(_Color, r, g, b, a);

  err := 0;
  err := circleRGBA(_Renderer, x, y, rad, r, g, b, a);
  if err <> 0 then
  begin
    errStr := 'Failed to call the DrawCircle()! SDL Error: %s';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;
end;

procedure TTexture.DrawCircleA(x, y, rad: integer);
var
  r, g, b, a: Byte;
  err: Integer;
  errStr: String;
begin
  __GetRGBA(_Color, r, g, b, a);

  err := 0;
  err := aacircleRGBA(_Renderer, x, y, rad, r, g, b, a);
  if err <> 0 then
  begin
    errStr := 'Failed to call the DrawCircleA()! SDL Error: %s';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;
end;

procedure TTexture.DrawCircleAndFilled(x, y, rad: integer);
var
  r, g, b, a: Byte;
  err: Integer;
  errStr: String;
begin
  __GetRGBA(_Color, r, g, b, a);

  err := 0;
  err := filledCircleRGBA(_Renderer, x, y, rad, r, g, b, a);
  if err <> 0 then
  begin
    errStr := 'Failed to call the DrawCircleAndFilled()! SDL Error: %s';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;
end;

function TTexture.GetScale: TTexture.TScale;
begin
  Result.x := _Scale.x;
  Result.y := _Scale.y;
end;

procedure TTexture.LoadFormString(ttfName: string; ttfSize: integer; Text: string; color: TSDL_Color);
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
    newTexture := SDL_CreateTextureFromSurface(_Renderer, textSurface);
    if newTexture = nil then
    begin
      errStr := 'Unable to create texture from rendered text! SDL Error: %s';
      errStr.Format([SDL_GetError()]);
      raise Exception.Create(errStr.ToAnsiString);
    end;

    // Get image dimensions
    _Width := textSurface^.w;
    _Height := textSurface^.h;
    _Texture := newTexture;
  finally
    TTF_CloseFont(font);
    SDL_FreeSurface(textSurface);
  end;
end;

procedure TTexture.LoadFromFile(path: string);
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
      newTexture := SDL_CreateTextureFromSurface(_Renderer, loadedSurface);
      if newTexture = nil then
      begin
        errStr := 'Unable to create texture from %s! SDL Error: %s';
        errStr.Format([fileName, SDL_GetError()]);
        raise Exception.Create(errStr.ToAnsiString);
      end;

      _Width := loadedSurface^.w;
      _Height := loadedSurface^.h;
      _Texture := newTexture;
    finally
      // Get rid of old loaded surface
      SDL_FreeSurface(loadedSurface);
    end;
  end;
end;

procedure TTexture.Render(x, y: integer; clip: PSDL_Rect; angle: double;
  center: PSDL_Point; flip: TSDL_RendererFlags);
var
  renderQuad: TSDL_Rect;
  err: Integer;
  errStr: String;
  scale: TScale;
begin
  // Set rendering space and render to screen
  renderQuad := Default(TSDL_Rect);
  renderQuad.x := x;
  renderQuad.y := y;
  renderQuad.w := _Width;
  renderQuad.h := _Height;

  // Set clip rendering dimensions
  if clip <> nil then
  begin
    renderQuad.w := clip^.w;
    renderQuad.h := clip^.h;
  end;

  err := 0;
  scale := Self.GetScale;

  err := SDL_RenderSetScale(_Renderer, scale.x, scale.y);
   if err <> 0 then
  begin
    errStr := 'RenderSetScale failure! SDL Error: %s';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;

  err := SDL_RenderCopyEx(_Renderer, _Texture, clip, @renderQuad, angle, center,
    flip);

  if err <> 0 then
  begin
    errStr := 'Render failure! SDL Error: %s';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;
end;

procedure TTexture.Render(srcRect: PSDL_Rect; destRect: PSDL_Rect);
var
  err: integer;
  errStr: String;
  scale: TScale;
begin
  err := 0;
  scale := Self.GetScale;

  err := SDL_RenderSetScale(_Renderer, scale.x, scale.y);
  if err <> 0 then
  begin
    errStr := 'RenderSetScale failure! SDL Error: %s';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;

  err := SDL_RenderCopy(_Renderer, _Texture, srcRect, destRect);
  if err <> 0 then
  begin
    errStr := 'Render failure! SDL Error: %s';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;
end;

procedure TTexture.Render(p: TPoint);
begin
  Self.Render(p.X, p.Y);
end;

procedure TTexture.SetTarget;
begin
  SDL_SetRenderTarget(_Renderer, _Texture);
end;

procedure TTexture.SetColorMod(color: TSDL_Color);
var
  r, g, b, a: Byte;
  err: integer;
  errStr: String;
begin
  __GetRGBA(color, r, g, b, a);

  err := SDL_SetTextureColorMod(_Texture, color.R, color.G, color.B);
  if err <> 0 then
  begin
    errStr := 'Failed to call SetColorMod()! SDL Error: %s';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;
end;

procedure TTexture.SetDrawColor(r, g, b, a: Byte);
var
  err: integer;
  errStr: String;
begin
  _Color.r := r;
  _Color.g := g;
  _Color.b := b;
  _Color.a := a;

  err := SDL_SetRenderDrawColor(_Renderer, r, g, b, a);
  if err <> 0 then
  begin
    errStr := 'Failed to call the SetDrawColor()! SDL Error: %s';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;
end;

procedure TTexture.SetDrawColor(color: TSDL_Color);
var
  a, r, g, b: Byte;
  err: integer;
  errStr: String;
begin
  _Color := color;
  __GetRGBA(color, r, g, b, a);

  err := SDL_SetRenderDrawColor(_Renderer, r, g, b, a);
  if err <> 0 then
  begin
    errStr := 'Failed to call the SetDrawColor()! SDL Error: %s';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;
end;

procedure TTexture.SetDrawColor;
begin
  _Color := TSDL_Color(TAlphaColors.White);
end;

procedure TTexture.SetPosition(ax, ay: integer);
begin
  _Position := TPoint.Create(ax, ay);
end;

procedure TTexture.SetPosition(ap: TPoint);
begin
  _Position := ap;
end;

procedure TTexture.SetRenderer(renderer: PSDL_Renderer);
begin
  _Renderer := renderer;
end;

procedure TTexture.SetScale(x, y: float);
begin
  _Scale.x := x;
  _Scale.y := y;
end;

function TTexture.ToPSDL_Texture: PSDL_Texture;
begin
  Result := _Texture;
end;

procedure TTexture.UnsetTarget;
var
  err: Integer;
  errStr: String;
begin
  err := 0;
  err := SDL_SetRenderTarget(_Renderer, nil);

  if err <> 0 then
  begin
    errStr := 'Unsetting the current texture to render target failed! SDL Error: %s';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;
end;

procedure TTexture.__Free;
begin
  if _Texture <> nil then
  begin
    SDL_DestroyTexture(_Texture);
    _Texture := nil;

    _Height := 0;
    _Width := 0;
  end;
end;

function TTexture.__GetBoundsRect: TRect;
begin
  Result := Rect
    (
      _Position.x,
      _Position.y,
      _Position.x + _Width,
      _Position.y + _Height
    );
end;

function TTexture.__GetData: PSDL_Texture;
begin
  Result := _Texture;
end;

function TTexture.__GetHeight: integer;
begin
  Result := _Height;
end;

function TTexture.__GetPosition: TPoint;
begin
  Result := _Position;
end;

procedure TTexture.__GetRGBA(color: TSDL_Color; out r, g, b, a: Byte);
begin
  r := color.r;
  g := color.g;
  b := color.b;
  a := color.a;
end;

function TTexture.__GetWidth: integer;
begin
  Result := _Width;
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
