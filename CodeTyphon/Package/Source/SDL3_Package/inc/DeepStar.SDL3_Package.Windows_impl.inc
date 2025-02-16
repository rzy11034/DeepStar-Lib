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
  _Window := __CreateWindow(caption, w, h, flags, false);
end;

procedure TWindow.InitWithOpenGL(caption: string; w, h: integer;
  flags: TSDL_WindowFlags);
begin
  flags := flags or SDL_WINDOW_OPENGL;
  _Window := __CreateWindow(caption, w, h, flags, true);
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

function TWindow.__CreateWindow(Caption: string; w, h: integer; flags: TSDL_WindowFlags;
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
  _WindowID := SDL_GetWindowID(res);
  _Width := w;
  _Height := h;
  _Caption := caption;
  _EnableOpenGL := enableOpenGL;

  if enableOpenGL then
  begin
    // 设置 OpenGL 属性
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 4);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 3);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE);
    SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
    SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 24);

    _Context := SDL_GL_CreateContext(res);
  end;

  Result := res;
end;

function TWindow.__GetCaption: string;
begin
  Result := _Caption;
end;

function TWindow.__GetGL_Context: TSDL_GLContext;
var
  errStr: String;
begin
  if not _EnableOpenGL then
  begin
    errStr := 'Windows don''t specify OpenGL!';
    raise Exception.Create(errStr.ToAnsiString);
  end;

  Result := _Context;
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

