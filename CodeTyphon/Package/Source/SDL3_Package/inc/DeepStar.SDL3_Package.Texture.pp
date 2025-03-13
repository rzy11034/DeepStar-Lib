
type
  TCustomImage = class (TInterfacedObject)
  protected class var
    fImageRefCount: integer;

  private
    procedure TTF_Initiate;

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
    fRenderer: PSDL_Renderer;
    fTexture: PSDL_Texture;
    fPosition: TPointF;
    fScale: TScale;

    procedure _Free;

    function GetBoundsRect: TRectF;
    function GetHeight: integer;
    function GetPosition: TPointF;
    function GetWidth: integer;

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

    property Width: integer read GetWidth;
    property Height: integer read GetHeight;
    property Position: TPointF read GetPosition;
    property BoundsRect: TRectF read GetBoundsRect;
  end;

type
  TArr_TTexture = array of TTexture;

