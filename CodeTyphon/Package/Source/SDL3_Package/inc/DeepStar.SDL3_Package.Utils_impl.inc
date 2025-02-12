

operator := (AColor: TColor): TSDL_Color;
var
  temp: TColors;
begin
  temp := TColors(AColor);

  Result.r := temp.R;
  Result.g := temp.G;
  Result.b := temp.B;
  Result.a := $FF;
end;

operator := (AColor: TAlphaColor): TSDL_Color;
var
  temp: TAlphaColors;
begin
  temp := TAlphaColors(AColor);

  Result.r := temp.R;
  Result.g := temp.G;
  Result.b := temp.B;
  Result.a := temp.A;
end;

operator := (AColor: TColor): TColors;
begin
  Result := AColor;
end;

operator := (AColor: TAlphaColor): TAlphaColors;
begin
  Result := TAlphaColors.Create(AColor);
end;

function PointF(x, y: Single): TPointF;
begin
  Result := TPointF.Create(x, y);
end;

function RectF(p: TPointF; w, h: Single): TRectF;
begin
  Result := TRectF.Create(p, w, h);
end;

function SDL_FPoint(x, y: float): TSDL_FPoint;
var
  res: TSDL_FPoint;
begin
  res := Default(TSDL_FPoint);

  res.x := x;
  res.y := y;

  Result := res;
end;

function SDL_FPoint(p: TPoint): TSDL_FPoint;
var
  res: TSDL_FPoint;
begin
  res := Default(TSDL_FPoint);

  res.x := p.X;
  res.y := p.Y;

  Result := res;
end;

function SDL_FRect(x, y, w, h: float): TSDL_FRect;
var
  res: TSDL_FRect;
begin
  res := Default(TSDL_FRect);

  res.x := x;
  res.y := y;
  res.w := w;
  res.h := h;

  Result := res;
end;

function SDL_FRect(rect: TRectF): TSDL_FRect;
var
  res: TSDL_FRect;
begin
  res := Default(TSDL_FRect);

  res.y := rect.Top;
  res.x := rect.Left;
  res.w := rect.Width;
  res.h := rect.Height;

  Result := res;
end;

function SDL_Color(r, g, b, a: byte): TSDL_Color;
var
  res: TSDL_Color;
begin
  res := Default(TSDL_Color);

  res.r := r;
  res.g := g;
  res.b := b;
  res.a := a;

  Result := res;
end;

function SDL_ColorF(r, g, b, a: single): TSDL_FColor;
var
  res: TSDL_FColor;
begin
  res := Default(TSDL_FColor);

  res.r := r;
  res.g := g;
  res.b := b;
  res.a := a;

  Result := res;
end;

{ TSDL_FPointHelper }

function TSDL_FPointHelper.ToPtr: PSDL_FPoint;
begin
  Result := @Self;
end;

{ TSDL_FRectHelper }

function TSDL_FRectHelper.ToPtr: PSDL_FRect;
begin
  Result := @Self;
end;

{ TSDL_ColorHelper }

function TSDL_ColorHelper.ToPtr: PSDL_Color;
begin
  Result := @Self;
end;

