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

  _Renderer := res;

  _Color := TSDL_Color(TAlphaColors.White);
  _ColorF := SDL_ColorF(1, 1, 1, 1);
end;

procedure TRenderer.Clear;
var
  errStr: String;
begin
  if not SDL_RenderClear(_Renderer) then
  begin
    errStr := 'Failed to SDL_RenderClear()! SDL Error: %s!';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;
end;

destructor TRenderer.Destroy;
begin
  if _Renderer <> nil then
  begin
    SDL_DestroyRenderer(_Renderer);
    _Renderer := nil;
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

  if not SDL_SetRenderScale(_Renderer, scale.x, scale.y) then
  begin
    errStr := 'Failed to SDL_SetRenderScale()! SDL Error: %s';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;

  if not SDL_RenderTextureRotated(_Renderer, texture.ToPSDL_Texture, clip,
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

  if not SDL_SetRenderScale(_Renderer, scale.x, scale.y) then
  begin
    errStr := 'Failed to SDL_SetRenderScale()! SDL Error: %s';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;

  if not SDL_RenderTexture(_Renderer, texture.ToPSDL_Texture, srcRect, destRect) then
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
  if not SDL_RenderPresent(_Renderer) then
  begin
    errStr := 'Failed to SDL_RenderPresent()! SDL Error: %s!';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;
end;

procedure TRenderer.SetDrawColor(r, g, b, a: Byte);
begin
  _Color := SDL_Color(r, g, b, a);

  __SetDrawColor;
  __GetDrawColorF;
end;

procedure TRenderer.SetDrawColor(color: TSDL_Color);
begin
  _Color := color;

  __SetDrawColor;
  __GetDrawColorF;
end;

procedure TRenderer.SetDrawColorFloat(r, g, b, a: Single);
begin
  _ColorF := SDL_ColorF(r, g, b, a);
  __SetDrawColorF;
  __GetDrawColor;
end;

procedure TRenderer.SetDrawColorFloat(color: TSDL_FColor);
begin
  _ColorF := color;
  __SetDrawColorF;
  __GetDrawColor;
end;

function TRenderer.ToPSDL_Renderer: PSDL_Renderer;
begin
  Result := _Renderer;
end;

procedure TRenderer.__GetDrawColor;
var
  r, g, b, a: byte;
  errStr: String;
begin
  if not SDL_GetRenderDrawColorFloat(_Renderer, @r, @g, @b, @a) then
  begin
    errStr := 'Failed to SDL_GetRenderDrawColorFloat()! SDL Error: %s!';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;

  _Color := SDL_Color(r, g, b, a);
end;

procedure TRenderer.__GetDrawColorF;
var
  r, g, b, a: single;
  errStr: String;
begin
  if not SDL_GetRenderDrawColorFloat(_Renderer, @r, @g, @b, @a) then
  begin
    errStr := 'Failed to SDL_GetRenderDrawColorFloat()! SDL Error: %s!';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;

  _ColorF := SDL_ColorF(r, g, b, a);
end;

procedure TRenderer.__SetDrawColor;
var
  r, g, b, a: byte;
  errStr: String;
begin
  r := _Color.r;
  g := _Color.g;
  b := _Color.b;
  a := _Color.a;

  if not SDL_SetRenderDrawColor(_Renderer, r, g, b, a) then
  begin
    errStr := 'Failed to SDL_SetRenderDrawColor()! SDL Error: %s!';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;
end;

procedure TRenderer.__SetDrawColorF;
var
  r, g, b, a: single;
  errStr: String;
begin
  r := _ColorF.r;
  g := _ColorF.g;
  b := _ColorF.b;
  a := _ColorF.a;

  if not SDL_SetRenderDrawColorFloat(_Renderer, r, g, b, a) then
  begin
    errStr := 'Failed to SDL_SetRenderDrawColorFloat()! SDL Error: %s!';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;
end;

