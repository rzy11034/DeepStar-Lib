{ TCustomImage }

constructor TCustomImage.Create;
begin
  if _ImageRefCount = 0 then
  begin
    __TTF_Init;
  end;

  _ImageRefCount += 1;
end;

destructor TCustomImage.Destroy;
begin
  _ImageRefCount -= 1;

  if _ImageRefCount <= 0 then
  begin
    TTF_Quit;
  end;

  inherited Destroy;
end;

procedure TCustomImage.__TTF_Init;
var
  errStr: string;
begin
  if not TTF_Init() then
  begin
    errStr := 'SDL_ttf could not initialize! SDL_ttf Error: %s';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;
end;

{ TTexture }

constructor TTexture.Create(renderer: PSDL_Renderer);
begin
  inherited Create;

  _Renderer := renderer;
end;

constructor TTexture.Create;
begin
  inherited Create;
end;

function TTexture.CreateBlank(width, Height: integer): Boolean;
var
  errStr: String;
begin
  __Free;

  _Texture := SDL_CreateTexture
  (
    _Renderer,
    SDL_PIXELFORMAT_RGBA8888,
    SDL_TEXTUREACCESS_TARGET,
    Width,
    Height
  );

  if _Texture = nil then
  begin
    errStr := 'Unable to create streamable blank texture! SDL Error: %s';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;

  Result := _Texture <> nil;
end;

destructor TTexture.Destroy;
begin
  Self.__Free;
  inherited Destroy;
end;

function TTexture.GetScale: TTexture.TScale;
begin
  Result.x := _Scale.x;
  Result.y := _Scale.y;
end;

procedure TTexture.LoadFormString(ttfName: string; ttfSize: integer; Text: string;
            color: TSDL_Color);
var
  font: PTTF_Font;
  errStr: String;
  textSurface: PSDL_Surface;
  newTexture: PSDL_Texture;
begin
  Self.__Free;

  font := PTTF_Font(nil);
  font := TTF_OpenFont(CrossFixFileName(ttfName).ToPAnsiChar, ttfSize);
  if font = nil then
  begin
    errStr := 'Failed to load font! SDL_ttf Error: %s';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;

  textSurface := PSDL_Surface(nil);
  try
    textSurface := TTF_RenderText_Blended(font, Text.ToPAnsiChar, Length(Text), color);

    if textSurface = nil then
    begin
      errStr := 'Unable to render text surface! SDL_ttf Error: %s';
      errStr.Format([SDL_GetError()]);
      raise Exception.Create(errStr.ToAnsiString);
    end;

    // Create texture from surface pixels
    newTexture := PSDL_Texture(nil);
    newTexture := SDL_CreateTextureFromSurface(_Renderer, textSurface);
    if newTexture = nil then
    begin
      errStr := 'Unable to create texture from rendered text! SDL Error: %s';
      errStr.Format([SDL_GetError()]);
      raise Exception.Create(errStr.ToAnsiString);
    end;

    // Get image
    _Texture := newTexture;
  finally
    TTF_CloseFont(font);
    SDL_DestroySurface(textSurface);
  end;
end;

procedure TTexture.LoadFromFile(path: string);
var
  fileName, errStr: String;
  newTexture: PSDL_Texture;
begin
  Self.__Free;
  fileName := CrossFixFileName(path);

  newTexture := PSDL_Texture(nil);
  newTexture := IMG_LoadTexture(_Renderer, fileName.ToPAnsiChar);

  if newTexture = nil then
  begin
    errStr := 'Unable to create texture from %s! SDL Error: %s';
    errStr.Format([fileName, SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;

  _Texture := newTexture;
end;

procedure TTexture.SetColorMod(color: TSDL_Color);
var
  errStr: String;
begin
  if not SDL_SetTextureColorMod(_Texture, color.r, color.g, color.b) then
  begin
    errStr := 'Failed to SDL_GetRenderDrawColorFloat()! SDL Error: %s!';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;
end;

procedure TTexture.SetPosition(x, y: Single);
begin
  _Position := TPointF.Create(x, y);
end;

procedure TTexture.SetPosition(p: TPointF);
begin
  Self.SetPosition(p.x, p.y);
end;

procedure TTexture.SetPosition(p: TSDL_FPoint);
begin
  Self.SetPosition(p.x, p.y);
end;

procedure TTexture.SetScale(x, y: Single);
begin
  _Scale.x := x;
  _Scale.y := y;
end;

procedure TTexture.SetScale(scale: TScale);
begin
  Self.SetScale(scale.x, scale.y);
end;

procedure TTexture.SetTarget;
var
  errStr: String;
begin
  if not SDL_SetRenderTarget(_Renderer, _Texture) then
  begin
    errStr := 'Failed to SDL_SetRenderTarget()! SDL Error: %s!';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;
end;

function TTexture.ToPSDL_Texture: PSDL_Texture;
begin
  Result := _Texture;
end;

procedure TTexture.UnsetTarget;
var
  errStr: String;
begin
  if not SDL_SetRenderTarget(_Renderer, nil) then
  begin
    errStr := 'Failed to SDL_SetRenderTarget()! SDL Error: %s';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;
end;

procedure TTexture.__Free;
begin
  if _Texture <> nil then
  begin
    SDL_DestroyTexture(_Texture);
    _Texture := nil;
  end;
end;

function TTexture.__GetBoundsRect: TRectF;
var
  res: TRectF;
begin
  res := Default(TRectF);

  res.Left := _Position.x;
  res.Top := _Position.y;
  res.Right := _Position.x + _Texture^.w;
  res.Bottom := _Position.y + _Texture^.h;

  Result := res;
end;

function TTexture.__GetHeight: integer;
begin
  Result := IfThen(_Texture <> nil, _Texture^.h, 0);
end;

function TTexture.__GetPosition: TPointF;
begin
  Result := _Position;
end;

function TTexture.__GetWidth: integer;
begin
  Result := IfThen(_Texture <> nil, _Texture^.w, 0);
end;

{ TTexture.TScale }

class operator TTexture.TScale.initialize(var obj: TScale);
begin
  obj.x := 1;
  obj.y := 1;
end;

class function TTexture.TScale.Create: TScale;
begin
  initialize(Result);
end;
