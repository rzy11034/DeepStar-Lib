{ TCustomWindow }

constructor TCustomWindow.Create;
begin
  inherited Create;

  if fWindowRefCount = 0 then
  begin
    SDL_Initiate;
  end;

  fWindowRefCount += 1;
end;

destructor TCustomWindow.Destroy;
begin
  fWindowRefCount -= 0;

  if fWindowRefCount <= 0 then
    SDL_Quit;

  inherited Destroy;
end;

procedure TCustomWindow.SDL_Initiate;
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

  fWindow := PSDL_Window(nil);
  fWindowID := 0;
  fWidth := 0;
  fHeight := 0;
end;

destructor TWindow.Destroy;
begin
  if fWindow <> nil then
  begin
    SDL_DestroyWindow(fWindow);
    fWindow := nil;
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
  fWindow := CreateWindow(caption, w, h, flags, false);
end;

procedure TWindow.InitWithOpenGL(caption: string; w, h: integer;
  flags: TSDL_WindowFlags);
begin
  flags := flags or SDL_WINDOW_OPENGL;
  fWindow := CreateWindow(caption, w, h, flags, true);
end;

procedure TWindow.Show;
var
  errStr: String;
begin
  if not SDL_ShowWindow(fWindow) then
  begin
    errStr := 'Failed to show the window! SDL Error: %s!';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;
end;

function TWindow.ShowMessageBox(flag: TMessageBoxType; title, message: string): Integer;
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
  mbd.message := message.ToPAnsiChar;
  mbd.window := Self.ToPSDL_Window;
  mbd.numbuttons := 2;
  mbd.Buttons := @mbts[0];
  mbd.colorScheme := nil;//@colorScheme;

  if SDL_ShowMessageBox(@mbd, @buttonID) then
    res := buttonID
  else
    res := -1;

  Result := res;
end;

function TWindow.ToPSDL_Window: PSDL_Window;
begin
  Result := fWindow;
end;

function TWindow.CreateRenderer: PSDL_Renderer;
var
  res: PSDL_Renderer;
begin
  res := PSDL_Renderer(nil);

  res := SDL_CreateRenderer(fWindow, nil);

  Result := res;
end;

function TWindow.CreateWindow(Caption: string; w, h: integer; flags: TSDL_WindowFlags;
  enableOpenGL: boolean): PSDL_Window;
var
  winFlags: Integer;
  res: PSDL_Window;
  errStr: String;
begin
  winFlags := 0;

  if enableOpenGL then
    winFlags := winFlags or SDL_WINDOW_OPENGL;

  winFlags := flags or SDL_WINDOW_HIDDEN;

  res := PSDL_Window(nil);

  res := SDL_CreateWindow(caption.ToPAnsiChar, w, h, winFlags);
  if res = nil then
  begin
    errStr := 'Window could not be created! SDL_Error: %s';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;

  // 获取窗口标识符
  fWindowID := SDL_GetWindowID(res);
  fWidth := w;
  fHeight := h;
  fCaption := caption;
  fEnableOpenGL := enableOpenGL;

  if enableOpenGL then
  begin
    // 设置 OpenGL 属性
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 4);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 3);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE);
    SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
    SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 24);

    fContext := SDL_GL_CreateContext(res);
  end;

  Result := res;
end;

function TWindow.GetCaption: string;
begin
  Result := fCaption;
end;

function TWindow.GetGL_Context: TSDL_GLContext;
var
  errStr: String;
begin
  if not fEnableOpenGL then
  begin
    errStr := 'Windows don''t specify OpenGL!';
    raise Exception.Create(errStr.ToAnsiString);
  end;

  Result := fContext;
end;

function TWindow.GetHeight: integer;
begin
  Result := fHeight;
end;

function TWindow.GetResizable: Boolean;
var
  flag: TSDL_WindowFlags;
  res: Boolean;
begin
  flag := SDL_GetWindowFlags(fWindow);
  res := false;

  If (flag and SDL_WINDOW_RESIZABLE) = SDL_WINDOW_RESIZABLE then
    res := true;

  Result := res;
end;

function TWindow.GetWidth: Integer;
begin
  Result := fWidth;
end;

procedure TWindow.SetCaption(const value: string);
begin
  SDL_SetWindowTitle(fWindow, value.ToPAnsiChar);
  fCaption := value;
end;

procedure TWindow.SetResizable(const value: Boolean);
begin
  SDL_SetWindowResizable(fWindow, value);
end;

