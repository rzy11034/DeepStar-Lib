unit DeepStar.SDL3_Package.Windows;

{$mode ObjFPC}{$H+}
{$ModeSwitch unicodestrings}{$J-}

interface

uses
  Classes,
  SysUtils,
  libSDL3,
  DeepStar.Utils,
  DeepStar.SDL3_Package.Utils;

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
    //Window data
    _Window: PSDL_Window;
    _WindowID: Cardinal;
    _Context: TSDL_GLContext;

    //Window dimensions
    _Width: Integer;
    _Height: Integer;
    _Caption: string;

    function __CreateWindow(caption: string; w, h: integer;
                 flags: TSDL_WindowFlags): PSDL_Window;
    function __CreateRenderer: PSDL_Renderer;
    function __GetCaption: string;
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

    function ToPSDL_Window: PSDL_Window;

    procedure Init(caption: string; w, h: integer; flags: TSDL_WindowFlags = 2);
    procedure Show;

    property Caption: string read __GetCaption write __SetCaption;
    property Width: Integer read __GetWidth;
    property Height: integer read __GetHeight;
    property Resizable: Boolean read __GetResizable write __SetResizable;
  end;

implementation

{ TCustomWindow }

constructor TCustomWindow.Create;
begin
  inherited Create;

  if _WindowRefCount = 0 then
  begin
    __SDL_Init;
  end;

  _WindowRefCount += 1;
end;

destructor TCustomWindow.Destroy;
begin
  _WindowRefCount -= 0;

  if _WindowRefCount <= 0 then
    SDL_Quit;

  inherited Destroy;
end;

procedure TCustomWindow.__SDL_Init;
var
  errStr: string;
begin
  if not SDL_Init(SDL_INIT_VIDEO) then
  begin
    errStr := 'SDL could not initialize! SDL_Error：%s';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;
end;

{ TWindow }

constructor TWindow.Create();
begin
  inherited Create;

  _Window := PSDL_Window(nil);
  _WindowID := 0;
  _Width := 0;
  _Height := 0;
end;

destructor TWindow.Destroy;
begin
  if _Window <> nil then
  begin
    SDL_DestroyWindow(_Window);
    _Window := nil;
  end;

  inherited Destroy;
end;

function TWindow.GetMousePos: TPointF;
var
  x, y: Single;
begin
  SDL_GetMouseState(@x, @y);
  Result := TPointF.Create(x, y);
end;

procedure TWindow.Init(caption: string; w, h: integer; flags: TSDL_WindowFlags);
begin
  _Window := __CreateWindow(caption, w, h, flags);
end;

procedure TWindow.Show;
var
  errStr: String;
begin
  if not SDL_ShowWindow(_Window) then
  begin
    errStr := 'Failed to show the window! SDL Error: %s!';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;
end;

function TWindow.ToPSDL_Window: PSDL_Window;
begin
  Result := _Window;
end;

function TWindow.__CreateRenderer: PSDL_Renderer;
var
  res: PSDL_Renderer;
begin
  res := PSDL_Renderer(nil);

  res := SDL_CreateRenderer(_Window, nil);

  Result := res;
end;

function TWindow.__CreateWindow(caption: string; w, h: integer;
  flags: TSDL_WindowFlags): PSDL_Window;
var
  winFlags: Integer;
  res: PSDL_Window;
  errStr: String;
begin
  winFlags := SDL_WINDOW_HIDDEN or SDL_WINDOW_OPENGL;
  winFlags := winFlags or flags;

  res := PSDL_Window(nil);

  res := SDL_CreateWindow(caption.ToPAnsiChar, w, h, winFlags);
  if res = nil then
  begin
    errStr := 'Window could not be created! SDL_Error: %s';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;

  // 获取窗口标识符
  _WindowID := SDL_GetWindowID(res);
  _Width := w;
  _Height := h;
  _Caption := caption;

  // 设置 OpenGL 属性
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 4);
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 3);
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE);
  SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
  SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 24);

  _Context := SDL_GL_CreateContext(res);

  Result := res;
end;

function TWindow.__GetCaption: string;
begin
  Result := _Caption;
end;

function TWindow.__GetHeight: integer;
begin
  Result := _Height;
end;

function TWindow.__GetResizable: Boolean;
var
  flag: TSDL_WindowFlags;
  res: Boolean;
begin
  flag := SDL_GetWindowFlags(_Window);
  res := false;

  If (flag and SDL_WINDOW_RESIZABLE) = SDL_WINDOW_RESIZABLE then
    res := true;

  Result := res;
end;

function TWindow.__GetWidth: Integer;
begin
  Result := _Width;
end;

procedure TWindow.__SetCaption(const value: string);
begin
  SDL_SetWindowTitle(_Window, value.ToPAnsiChar);
  _Caption := value;
end;

procedure TWindow.__SetResizable(const value: Boolean);
begin
  SDL_SetWindowResizable(_Window, value);
end;

end.

