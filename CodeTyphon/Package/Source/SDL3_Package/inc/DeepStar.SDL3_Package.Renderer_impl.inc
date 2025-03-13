{ TRenderer }

constructor TRenderer.Create(win: PSDL_Window; name: string);
var
  res: PSDL_Renderer;
  errStr: String;
begin
  res := PSDL_Renderer(nil);
  res := SDL_CreateRenderer(win, name.ToPAnsiChar);

  if res = nil then
  begin
    errStr := 'Failed to create renderer! SDL Error: %s!';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;

  fRenderer := res;

  fColor := TSDL_Color(TAlphaColors.White);
  fColorF := SDL_ColorF(1, 1, 1, 1);
end;

procedure TRenderer.Clear;
var
  errStr: String;
begin
  if not SDL_RenderClear(fRenderer) then
  begin
    errStr := 'Failed to SDL_RenderClear()! SDL Error: %s!';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;
end;

destructor TRenderer.Destroy;
begin
  if fRenderer <> nil then
  begin
    SDL_DestroyRenderer(fRenderer);
    fRenderer := nil;
  end;

  inherited Destroy;
end;

procedure TRenderer.Render(texture: TTexture);
var
  p: TPointF;
begin
  p := texture.Position;
  Self.Render(texture, p);
end;

procedure TRenderer.Render(texture: TTexture; x, y: Single; clip: PSDL_FRect;
  angle: double; center: PSDL_FPoint; flip: TSDL_FlipMode);
var
  renderQuad: TSDL_FRect;
  h, w: Integer;
  scale: TTexture.TScale;
  errStr: String;
begin
  w := texture.Width;
  h := texture.Height;
  scale := texture.GetScale;

  // 设置渲染空间并渲染到屏幕
  renderQuad := Default(TSDL_FRect);
  renderQuad.x := x;
  renderQuad.y := y;
  renderQuad.w := w;
  renderQuad.h := h;

  // 设置剪辑渲染尺寸
  if clip <> nil then
  begin
    renderQuad.w := clip^.w;
    renderQuad.h := clip^.h;
  end;

  if not SDL_SetRenderScale(fRenderer, scale.x, scale.y) then
  begin
    errStr := 'Failed to SDL_SetRenderScale()! SDL Error: %s';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;

  if not SDL_RenderTextureRotated(fRenderer, texture.ToPSDL_Texture, clip,
    @renderQuad, angle, center, flip) then
  begin
    errStr := 'Failed to SDL_RenderTextureRotated()! SDL Error: %s';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;
end;

procedure TRenderer.Render(texture: TTexture; srcRect: PSDL_FRect; destRect: PSDL_FRect);
var
  scale: TTexture.TScale;
  errStr: String;
begin
  scale := texture.GetScale;

  if not SDL_SetRenderScale(fRenderer, scale.x, scale.y) then
  begin
    errStr := 'Failed to SDL_SetRenderScale()! SDL Error: %s';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;

  if not SDL_RenderTexture(fRenderer, texture.ToPSDL_Texture, srcRect, destRect) then
  begin
    errStr := 'Failed to SDL_RenderTexture()! SDL Error: %s';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;
end;

procedure TRenderer.Render(texture: TTexture; p: TPointF);
begin
  Self.Render(texture, p.x, p.y, nil, 0, nil, 0);
end;

procedure TRenderer.RenderPresent;
var
  errStr: String;
begin
  if not SDL_RenderPresent(fRenderer) then
  begin
    errStr := 'Failed to SDL_RenderPresent()! SDL Error: %s!';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;
end;

procedure TRenderer.SetDrawColor(r, g, b, a: Byte);
begin
  fColor := SDL_Color(r, g, b, a);

  SetDrawColor;
  GetDrawColorF;
end;

procedure TRenderer.SetDrawColor(color: TSDL_Color);
begin
  fColor := color;

  SetDrawColor;
  GetDrawColorF;
end;

procedure TRenderer.SetDrawColorFloat(r, g, b, a: Single);
begin
  fColorF := SDL_ColorF(r, g, b, a);
  SetDrawColorF;
  GetDrawColor;
end;

procedure TRenderer.SetDrawColorFloat(color: TSDL_FColor);
begin
  fColorF := color;
  SetDrawColorF;
  GetDrawColor;
end;

function TRenderer.ToPSDL_Renderer: PSDL_Renderer;
begin
  Result := fRenderer;
end;

procedure TRenderer.GetDrawColor;
var
  r, g, b, a: byte;
  errStr: String;
begin
  if not SDL_GetRenderDrawColorFloat(fRenderer, @r, @g, @b, @a) then
  begin
    errStr := 'Failed to SDL_GetRenderDrawColorFloat()! SDL Error: %s!';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;

  fColor := SDL_Color(r, g, b, a);
end;

procedure TRenderer.GetDrawColorF;
var
  r, g, b, a: single;
  errStr: String;
begin
  if not SDL_GetRenderDrawColorFloat(fRenderer, @r, @g, @b, @a) then
  begin
    errStr := 'Failed to SDL_GetRenderDrawColorFloat()! SDL Error: %s!';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;

  fColorF := SDL_ColorF(r, g, b, a);
end;

procedure TRenderer.SetDrawColor;
var
  r, g, b, a: byte;
  errStr: String;
begin
  r := fColor.r;
  g := fColor.g;
  b := fColor.b;
  a := fColor.a;

  if not SDL_SetRenderDrawColor(fRenderer, r, g, b, a) then
  begin
    errStr := 'Failed to SDL_SetRenderDrawColor()! SDL Error: %s!';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;
end;

procedure TRenderer.SetDrawColorF;
var
  r, g, b, a: single;
  errStr: String;
begin
  r := fColorF.r;
  g := fColorF.g;
  b := fColorF.b;
  a := fColorF.a;

  if not SDL_SetRenderDrawColorFloat(fRenderer, r, g, b, a) then
  begin
    errStr := 'Failed to SDL_SetRenderDrawColorFloat()! SDL Error: %s!';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;
end;

