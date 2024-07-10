unit DeepStar.SDL2_ADV.Windows;

{$mode ObjFPC}{$H+}
{$ModeSwitch unicodestrings}{$J-}

interface

uses
  Classes,
  SysUtils,
  DeepStar.SDL2_ADV.Texture,
  DeepStar.Utils,
  libSDL2,
  libSDL2_image,
  libSDL2_ttf;

type
  TMessageBoxType = type int32;

const
  MESSAGEBOX_ERROR = SDL_MESSAGEBOX_ERROR;
  MESSAGEBOX_WARNING = SDL_MESSAGEBOX_WARNING;
  MESSAGEBOX_INFORMATION = SDL_MESSAGEBOX_INFORMATION;

  SDL_COLOR_WHITE: TSDL_Color = (r: $FF; g: $FF; b: $FF; a: $FF);

type
  TWindow = class(TObject)
  private
    //Window data
    _PWindow: PSDL_Window;
    _PRenderer: PSDL_Renderer;
    _WindowID: uint32;

    //Window dimensions
    _Width: int32;
    _Height: int32;
    _Caption: string;

    function __CreateRenderer: PSDL_Renderer;
    function __CreateWindow(Caption: string; winPosX, winPosY, Width, Height: int32;
      flags: uint32): PSDL_Window;
    function __GetCaption: string;
    function __GetClientBounds: TRect;
    function __GetHeight: int32;
    function __GetPRenderer: PSDL_Renderer;
    function __GetPWindow: PSDL_Window;
    function __GetWidth: int32;

    procedure __SDL_Init;
    procedure __IMG_Init;
    procedure __TTF_Init;
    procedure __SDL_SetHint;
    procedure __SetCaption(const Value: string);

  public
    constructor Create;
    destructor Destroy; override;

    function ShowMessageBox(flag: TMessageBoxType; title, message: string): int32;
    // 获取鼠标相对窗口的坐标
    function GetMousePos: TPoint;

    procedure Init(Caption: string; Width, Height: uint32);
    procedure Init(Caption: string; winPosX, winPosY, Width, Height: int32; flags: uint32);
    procedure InitWithOpenGL(Caption: string; Width, Height: uint32);
    procedure Draw(const texture: TTexture);
    procedure Draw(const texture: TTexture; destRect: TRect);
    procedure Draw(const texture: TTexture; srcRect, destRect: TRect);
    procedure Display;
    procedure SetRenderDrawColorAndClear;
    procedure SetRenderDrawColorAndClear(color: TSDL_Color);

    property PWindow: PSDL_Window read __GetPWindow;
    property PRenderer: PSDL_Renderer read __GetPRenderer;
    property Width: int32 read __GetWidth;
    property Height: int32 read __GetHeight;
    property Caption: string read __GetCaption write __SetCaption;
    property ClientBounds: TRect read __GetClientBounds;
  end;

implementation

uses
  DeepStar.SDL2_ADV.Utils;

  { TWindow }

constructor TWindow.Create;
begin
  inherited Create;

  _PWindow := PSDL_Window(nil);
  _PRenderer := PSDL_Renderer(nil);
  _WindowID := 0;
  _Width := 0;
  _Height := 0;
end;

destructor TWindow.Destroy;
begin
  if _PRenderer <> nil then
  begin
    SDL_DestroyRenderer(_PRenderer);
    _PRenderer := nil;
  end;

  if _PWindow <> nil then
  begin
    SDL_DestroyWindow(_PWindow);
    _PWindow := nil;
  end;

  TTF_Quit;
  IMG_Quit;
  SDL_Quit;

  inherited Destroy;
end;

procedure TWindow.Display;
begin
  SDL_RenderPresent(_PRenderer);
end;

procedure TWindow.Draw(const texture: TTexture);
var
  destRect: TRect;
  scale: TTexture.TScale;
begin
  destRect := Bounds(
    texture.Position.x,
    texture.Position.y,
    texture.Width,
    texture.Height);

  scale := texture.GetScale;
  SDL_RenderSetScale(_PRenderer, scale.x, scale.y);

  SDL_RenderCopy(_PRenderer, texture.Data, nil, SDL_Rect(destRect).ToPtr);
end;

procedure TWindow.Draw(const texture: TTexture; srcRect, destRect: TRect);
var
  srcP, destP: PSDL_Rect;
  scale: TTexture.TScale;
begin
  scale := texture.GetScale;
  SDL_RenderSetScale(_PRenderer, scale.x, scale.y);

  srcP := SDL_Rect(srcRect).ToPtr;
  destP := SDL_Rect(destRect).ToPtr;
  SDL_RenderCopy(_PRenderer, texture.Data, srcP, destP);
end;

procedure TWindow.Draw(const texture: TTexture; destRect: TRect);
var
  scale: TTexture.TScale;
begin
  scale := texture.GetScale;
  SDL_RenderSetScale(_PRenderer, scale.x, scale.y);

  SDL_RenderCopy(_PRenderer, texture.Data, nil, SDL_Rect(destRect).ToPtr);
end;

function TWindow.GetMousePos: TPoint;
var
  x, y: integer;
begin
  x := 0;
  y := 0;
  SDL_GetMouseState(@x, @y);

  Result := Point(x, y);
end;

procedure TWindow.Init(Caption: string; winPosX, winPosY, Width, Height: int32; flags: uint32);
begin
  __SDL_Init;
  __SDL_SetHint;
  __IMG_Init;
  __TTF_Init;

  _Caption := Caption;

  _PWindow := __CreateWindow(Caption, winPosX, winPosY, Width, Height, flags);
  _Width := Width;
  _Height := Height;

  _PRenderer := __CreateRenderer;
end;

procedure TWindow.Init(Caption: string; Width, Height: uint32);
begin
  Self.Init(
    Caption,
    SDL_WINDOWPOS_UNDEFINED,
    SDL_WINDOWPOS_UNDEFINED,
    Width,
    Height,
    SDL_WINDOW_SHOWN);
end;

procedure TWindow.InitWithOpenGL(Caption: string; Width, Height: uint32);
begin
  Self.Init(
    Caption,
    SDL_WINDOWPOS_UNDEFINED,
    SDL_WINDOWPOS_UNDEFINED,
    Width,
    Height,
    SDL_WINDOW_SHOWN or SDL_WINDOW_OPENGL);
end;

procedure TWindow.SetRenderDrawColorAndClear(color: TSDL_Color);
begin
  SDL_SetRenderDrawColor(PRenderer, color.r, color.g, color.b, color.a);
  SDL_RenderClear(PRenderer);
end;

procedure TWindow.SetRenderDrawColorAndClear;
begin
  Self.SetRenderDrawColorAndClear(SDL_COLOR_WHITE);
end;

function TWindow.ShowMessageBox(flag: TMessageBoxType; title, message: string): int32;
  function __SDL_MessageBoxColor__(r, g, b: uint8): TSDL_MessageBoxColor;
  var
    res: TSDL_MessageBoxColor;
  begin
    res := Default(TSDL_MessageBoxColor);

    res.r := r;
    res.b := b;
    res.g := g;

    Result := res;
  end;

var
  mbd: TSDL_MessageBoxData;
  mbts: array of TSDL_MessageBoxButtonData;
  i, buttonID, res: integer;
  // colorScheme: TSDL_MessageBoxColorScheme;
begin
  mbd := Default(TSDL_MessageBoxData);

  SetLength(mbts, 2);
  for i := 0 to High(mbts) do
    mbts[i] := Default(TSDL_MessageBoxButtonData);

  //// [SDL_MESSAGEBOX_COLOR_BACKGROUND] */
  //colorScheme.colors[0] := __SDL_MessageBoxColor__(200, 200, 200);
  //// [SDL_MESSAGEBOX_COLOR_TEXT] */
  //colorScheme.colors[0] := __SDL_MessageBoxColor__(0, 0, 0);
  //// [SDL_MESSAGEBOX_COLOR_BUTTON_BORDER] */
  //colorScheme.colors[0] := __SDL_MessageBoxColor__(255, 255, 255);
  //// [SDL_MESSAGEBOX_COLOR_BUTTON_BACKGROUND] */
  //colorScheme.colors[0] := __SDL_MessageBoxColor__(150, 150, 150);
  //// [SDL_MESSAGEBOX_COLOR_BUTTON_SELECTED] */
  //colorScheme.colors[0] := __SDL_MessageBoxColor__(255, 255, 255);

  //////////////////////////////////

  with mbts[0] do
  begin
    flags := SDL_MESSAGEBOX_BUTTON_ESCAPEKEY_DEFAULT;
    buttonid := 2; {**< User defined button id (value returned via SDL_ShowMessageBox) *}
    Text := PAnsiChar('否(Esc)');
  end;

  with mbts[1] do
  begin
    flags := SDL_MESSAGEBOX_BUTTON_RETURNKEY_DEFAULT;
    buttonid := 1; {**< User defined button id (value returned via SDL_ShowMessageBox) *}
    Text := '是(Enter)';
  end;

  mbd.flags := flag;
  mbd.title := title.ToPAnsiChar;
  mbd._message := message.ToPAnsiChar;
  mbd.window := Self.PWindow;
  mbd.numbuttons := 2;
  mbd.Buttons := @mbts[0];
  mbd.colorScheme := nil;//@colorScheme;

  if SDL_ShowMessageBox(@mbd, @buttonID) >= 0 then
    res := buttonID
  else
    res := -1;

  Result := res;
end;

procedure TWindow.__IMG_Init;
var
  errStr: string;
begin
  if IMG_Init(IMG_INIT_PNG) < 0 then
  begin
    errStr := 'SDL_image could not initialize! SDL_image Error.';
    raise Exception.Create(errStr.ToAnsiString);
  end;
end;

procedure TWindow.__SDL_SetHint;
var
  errStr: string;
begin
  // Set texture filtering to linear
  if not SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, '1') then
  begin
    errStr := 'Warning: Linear texture filtering not enabled!';
    raise Exception.Create(errStr.ToAnsiString);
  end;
end;

procedure TWindow.__SetCaption(const Value: string);
begin
  if _Caption = Value then Exit;

  _Caption := Value;
  SDL_SetWindowTitle(_PWindow, Value.ToPAnsiChar);
end;

procedure TWindow.__TTF_Init;
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

procedure TWindow.__SDL_Init;
var
  errStr: string;
begin
  if SDL_Init(SDL_INIT_VIDEO) < 0 then
  begin
    errStr := 'SDL could not initialize! SDL_Error：%s';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;
end;

function TWindow.__CreateRenderer: PSDL_Renderer;
var
  errStr: string;
  res: PSDL_Renderer;
begin
  res := PSDL_Renderer(nil);

  // Create renderer for window
  res := SDL_CreateRenderer(_PWindow, -1, SDL_RENDERER_ACCELERATED);
  if res = nil then
  begin
    errStr := 'Renderer could not be created! SDL Error:';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;

  SDL_SetRenderDrawColor(res, $FF, $FF, $FF, $FF);

  Result := res;
end;

function TWindow.__CreateWindow(Caption: string; winPosX, winPosY, Width, Height: int32;
  flags: uint32): PSDL_Window;
var
  errStr: string;
  res: PSDL_Window;
begin
  res := PSDL_Window(nil);

  // Create window
  res := SDL_CreateWindow(Caption.ToPAnsiChar, winPosX, winPosY, Width, Height, flags);

  if res = nil then
  begin
    errStr := 'Window could not be created! SDL_Error: %s';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;

  // 获取窗口标识符
  _WindowID := SDL_GetWindowID(res);

  Result := res;
end;

function TWindow.__GetCaption: string;
begin
  Result := _Caption;
end;

function TWindow.__GetClientBounds: TRect;
begin
  Result := TRect.Create(0, 0, _Width, _Height);
end;

function TWindow.__GetHeight: int32;
begin
  Result := _Height;
end;

function TWindow.__GetPRenderer: PSDL_Renderer;
begin
  Result := _PRenderer;
end;

function TWindow.__GetPWindow: PSDL_Window;
begin
  Result := _PWindow;
end;

function TWindow.__GetWidth: int32;
begin
  Result := _Width;
end;

end.
