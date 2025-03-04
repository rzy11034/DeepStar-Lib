type
  TMessageBoxType = type int32;

const
  MESSAGEBOX_ERROR = libSDL3.SDL_MESSAGEBOX_ERROR;
  MESSAGEBOX_WARNING = libSDL3.SDL_MESSAGEBOX_WARNING;
  MESSAGEBOX_INFORMATION = libSDL3.SDL_MESSAGEBOX_INFORMATION;

type
  TCustomWindow = class abstract(TInterfacedObject)
  protected class var
    fWindowRefCount: integer;

  private
    procedure SDL_Initiate;

  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TWindow = class(TCustomWindow)
  private
    fEnableOpenGL: Boolean;
    //Window data
    fWindow: PSDL_Window;
    fWindowID: Cardinal;
    fContext: TSDL_GLContext;

    //Window dimensions
    fWidth: Integer;
    fHeight: Integer;
    fCaption: string;

    function CreateWindow(Caption: string; w, h: integer; flags: TSDL_WindowFlags;
              enableOpenGL: boolean): PSDL_Window;

    function CreateRenderer: PSDL_Renderer;
    function GetCaption: string;
    function GetGL_Context: TSDL_GLContext;
    function GetHeight: integer;
    function GetResizable: Boolean;
    function GetWidth: Integer;
    procedure SetCaption(const value: string);
    procedure SetResizable(const value: Boolean);

  public
    constructor Create; override;
    destructor Destroy; override;

    // 获取鼠标相对窗口的坐标
    function GetMousePos: TPointF;

    function ShowMessageBox(flag: TMessageBoxType; title, message: string): Integer;

    function ToPSDL_Window: PSDL_Window;

    procedure Init
              (
                caption: string;
                w, h: integer;
                flags: TSDL_WindowFlags = SDL_WINDOW_HIDDEN
              );

    procedure InitWithOpenGL
              (
                caption: string;
                w, h: integer;
                flags: TSDL_WindowFlags = SDL_WINDOW_HIDDEN
              );

    procedure Show;

    property Caption: string read GetCaption write SetCaption;
    property Width: Integer read GetWidth;
    property Height: integer read GetHeight;
    property Resizable: Boolean read GetResizable write SetResizable;
    property GL_Context: TSDL_GLContext read GetGL_Context;
  end;

