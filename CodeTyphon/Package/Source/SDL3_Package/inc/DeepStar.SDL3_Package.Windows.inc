type
  TMessageBoxType = type int32;

const
  MESSAGEBOX_ERROR = libSDL3.SDL_MESSAGEBOX_ERROR;
  MESSAGEBOX_WARNING = libSDL3.SDL_MESSAGEBOX_WARNING;
  MESSAGEBOX_INFORMATION = libSDL3.SDL_MESSAGEBOX_INFORMATION;

type
  TCustomWindow = class abstract(TInterfacedObject)
  protected class var
    _WindowRefCount: integer;

  private
    procedure __SDL_Init;

  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TWindow = class(TCustomWindow)
  private
    _EnableOpenGL: Boolean;
    //Window data
    _Window: PSDL_Window;
    _WindowID: Cardinal;
    _Context: TSDL_GLContext;

    //Window dimensions
    _Width: Integer;
    _Height: Integer;
    _Caption: string;

    function __CreateWindow(Caption: string; w, h: integer; flags: TSDL_WindowFlags;
              enableOpenGL: boolean): PSDL_Window;

    function __CreateRenderer: PSDL_Renderer;
    function __GetCaption: string;
    function __GetGL_Context: TSDL_GLContext;
    function __GetHeight: integer;
    function __GetResizable: Boolean;
    function __GetWidth: Integer;

    procedure __SetCaption(const value: string);
    procedure __SetResizable(const value: Boolean);

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

    property Caption: string read __GetCaption write __SetCaption;
    property Width: Integer read __GetWidth;
    property Height: integer read __GetHeight;
    property Resizable: Boolean read __GetResizable write __SetResizable;
    property GL_Context: TSDL_GLContext read __GetGL_Context;
  end;

