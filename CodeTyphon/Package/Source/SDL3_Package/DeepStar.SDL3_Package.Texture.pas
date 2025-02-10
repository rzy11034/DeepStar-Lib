unit DeepStar.SDL3_Package.Texture;

{$mode ObjFPC}{$H+}
{$ModeSwitch unicodestrings}{$J-}
{$ModeSwitch advancedrecords}

interface

uses
  Classes,
  SysUtils,
  libSDL3,
  libSDL3_image,
  libSDL3_ttf,
  DeepStar.Utils,
  DeepStar.SDL3_Package.Utils;

type
  TCustomImage = class (TInterfacedObject)
  protected class var
    _ImageRefCount: integer;

  private
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
    _Clip: PSDL_FRect;
    _Center: PSDL_FPoint;
    _Angle: Single;

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
    procedure LoadFormString(ttfName: string; ttfSize: integer;
                Text: string; color: TSDL_Color);

    // 渲染纹理
    procedure Render;
    procedure Render(srcRect: PSDL_FRect; destRect: PSDL_FRect);
    // 在给定点渲染纹理
    procedure Render(p: TPointF);

    procedure SetRenderer(renderer: PSDL_Renderer);
    procedure SetPosition(x, y: integer);
    procedure SetPosition(p: TPoint);
    procedure SetPosition(p: TSDL_Point);
    procedure SetColorMod(color: TSDL_Color);
    procedure SetScale(scale: TScale);
    procedure SetScale(x, y: Single);
    procedure SetClip(clip: PSDL_FRect);
    procedure SetCenter(center: PSDL_FPoint);
    procedure SetRotation(angle: Single);

    procedure SetTarget;
    procedure UnsetTarget;

    //(*═══════════════════════════════════════════════════════════════════════
    // 绘图函数

    //procedure Clear;
    //
    //procedure SetDrawColor;
    //procedure SetDrawColor(color: TSDL_Color);
    //procedure SetDrawColor(r, g, b, a: Byte);
    //
    //procedure DrawCircle(x, y, rad: integer);
    //procedure DrawCircleA(x, y, rad: integer);
    //procedure DrawCircleAndFilled(x, y, rad: integer);

    //═══════════════════════════════════════════════════════════════════════*)

    property Width: integer read __GetWidth;
    property Height: integer read __GetHeight;
    property Position: TPoint read __GetPosition;
    property BoundsRect: TRect read __GetBoundsRect;
  end;

type
  TArr_TTexture = array of TTexture;

implementation

{ TCustomImage }

constructor TCustomImage.Create;
begin
  if _ImageRefCount = 0 then
  begin
    __TTF_Init;
  end;

  _ImageRefCount += 1;
end;

destructor TCustomImage.Destroy;
begin
  _ImageRefCount -= 1;

  if _ImageRefCount <= 0 then
  begin
    TTF_Quit;
  end;

  inherited Destroy;
end;

procedure TCustomImage.__TTF_Init;
var
  errStr: string;
begin
  if not TTF_Init() then
  begin
    errStr := 'SDL_ttf could not initialize! SDL_ttf Error: %s';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;
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

