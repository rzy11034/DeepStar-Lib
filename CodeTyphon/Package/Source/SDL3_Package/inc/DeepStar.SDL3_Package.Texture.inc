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
    _Renderer: PSDL_Renderer;
    _Texture: PSDL_Texture;
    _Position: TPointF;
    _Scale: TScale;

    procedure __Free;
    function __GetBoundsRect: TRectF;
    function __GetHeight: integer;
    function __GetPosition: TPointF;
    function __GetWidth: integer;

  public
    constructor Create; override;
    constructor Create(renderer: PSDL_Renderer);
    destructor Destroy; override;

    // 新建一个空白纹理
    function CreateBlank(width, Height: integer): Boolean;

    function GetScale: TTexture.TScale;
    function ToPSDL_Texture: PSDL_Texture;

    // 指定路径图像创建纹理
    procedure LoadFromFile(path: string);

    // 从字符串创建纹理
    procedure LoadFormString(ttfName: string; ttfSize: integer;
                Text: string; color: TSDL_Color);

    procedure SetColorMod(color: TSDL_Color);
    procedure SetPosition(x, y: Single);
    procedure SetPosition(p: TPointF);
    procedure SetPosition(p: TSDL_FPoint);
    procedure SetRenderer(renderer: PSDL_Renderer);
    procedure SetScale(scale: TScale);
    procedure SetScale(x, y: Single);

    procedure SetTarget;
    procedure UnsetTarget;

    property Width: integer read __GetWidth;
    property Height: integer read __GetHeight;
    property Position: TPointF read __GetPosition;
    property BoundsRect: TRectF read __GetBoundsRect;
  end;

type
  TArr_TTexture = array of TTexture;

