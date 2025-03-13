{ TCustomImage }

constructor TCustomImage.Create;
begin
  if fImageRefCount = 0 then
  begin
    TTF_Initiate;
  end;

  fImageRefCount += 1;
end;

destructor TCustomImage.Destroy;
begin
  fImageRefCount -= 1;

  if fImageRefCount <= 0 then
  begin
    TTF_Quit;
  end;

  inherited Destroy;
end;

procedure TCustomImage.TTF_Initiate;
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

  fRenderer := renderer;
end;

constructor TTexture.Create;
begin
  inherited Create;
end;

function TTexture.CreateBlank(width, Height: integer): Boolean;
var
  errStr: String;
begin
  _Free;

  fTexture := SDL_CreateTexture
  (
    fRenderer,
    SDL_PIXELFORMAT_RGBA8888,
    SDL_TEXTUREACCESS_TARGET,
    Width,
    Height
  );

  if fTexture = nil then
  begin
    errStr := 'Unable to create streamable blank texture! SDL Error: %s';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;

  Result := fTexture <> nil;
end;

destructor TTexture.Destroy;
begin
  Self._Free;
  inherited Destroy;
end;

function TTexture.GetScale: TTexture.TScale;
begin
  Result.x := fScale.x;
  Result.y := fScale.y;
end;

procedure TTexture.LoadFormString(ttfName: string; ttfSize: integer; Text: string;
            color: TSDL_Color);
var
  font: PTTF_Font;
  errStr: String;
  textSurface: PSDL_Surface;
  newTexture: PSDL_Texture;
begin
  Self._Free;

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
    newTexture := SDL_CreateTextureFromSurface(fRenderer, textSurface);
    if newTexture = nil then
    begin
      errStr := 'Unable to create texture from rendered text! SDL Error: %s';
      errStr.Format([SDL_GetError()]);
      raise Exception.Create(errStr.ToAnsiString);
    end;

    // Get image
    fTexture := newTexture;
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
  Self._Free;
  fileName := CrossFixFileName(path);

  newTexture := PSDL_Texture(nil);
  newTexture := IMG_LoadTexture(fRenderer, fileName.ToPAnsiChar);

  if newTexture = nil then
  begin
    errStr := 'Unable to create texture from %s! SDL Error: %s';
    errStr.Format([fileName, SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;

  fTexture := newTexture;
end;

procedure TTexture.SetColorMod(color: TSDL_Color);
var
  errStr: String;
begin
  if not SDL_SetTextureColorMod(fTexture, color.r, color.g, color.b) then
  begin
    errStr := 'Failed to SDL_GetRenderDrawColorFloat()! SDL Error: %s!';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;
end;

procedure TTexture.SetPosition(x, y: Single);
begin
  fPosition := TPointF.Create(x, y);
end;

procedure TTexture.SetPosition(p: TPointF);
begin
  Self.SetPosition(p.x, p.y);
end;

procedure TTexture.SetPosition(p: TSDL_FPoint);
begin
  Self.SetPosition(p.x, p.y);
end;

procedure TTexture.SetRenderer(renderer: PSDL_Renderer);
begin
  fRenderer := renderer;
end;

procedure TTexture.SetScale(x, y: Single);
begin
  fScale.x := x;
  fScale.y := y;
end;

procedure TTexture.SetScale(scale: TScale);
begin
  Self.SetScale(scale.x, scale.y);
end;

procedure TTexture.SetTarget;
var
  errStr: String;
begin
  if not SDL_SetRenderTarget(fRenderer, fTexture) then
  begin
    errStr := 'Failed to SDL_SetRenderTarget()! SDL Error: %s!';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;
end;

function TTexture.ToPSDL_Texture: PSDL_Texture;
begin
  Result := fTexture;
end;

procedure TTexture.UnsetTarget;
var
  errStr: String;
begin
  if not SDL_SetRenderTarget(fRenderer, nil) then
  begin
    errStr := 'Failed to SDL_SetRenderTarget()! SDL Error: %s';
    errStr.Format([SDL_GetError()]);
    raise Exception.Create(errStr.ToAnsiString);
  end;
end;

procedure TTexture._Free;
begin
  if fTexture <> nil then
  begin
    SDL_DestroyTexture(fTexture);
    fTexture := nil;
  end;
end;

function TTexture.GetBoundsRect: TRectF;
var
  res: TRectF;
begin
  res := Default(TRectF);

  res.Left := fPosition.x;
  res.Top := fPosition.y;
  res.Right := fPosition.x + fTexture^.w;
  res.Bottom := fPosition.y + fTexture^.h;

  Result := res;
end;

function TTexture.GetHeight: integer;
begin
  Result := IfThen(fTexture <> nil, fTexture^.h, 0);
end;

function TTexture.GetPosition: TPointF;
begin
  Result := fPosition;
end;

function TTexture.GetWidth: integer;
begin
  Result := IfThen(fTexture <> nil, fTexture^.w, 0);
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
