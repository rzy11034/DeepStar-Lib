unit DeepStar.SDL3_Package.Utils;

{$mode ObjFPC}{$H+}
{$ModeSwitch unicodestrings}{$J-}
{$ModeSwitch advancedrecords}
{$modeswitch typehelpers}

interface

uses
  Classes,
  SysUtils,
  Types,
  libSDL3,
  System.UITypes,
  DeepStar.Utils,
  DeepStar.DSA.Linear.ArrayList;

type
  float = single;

  TColor = System.UITypes.TColor;
  TColors = System.UITypes.TColors;
  TAlphaColor = System.UITypes.TAlphaColor;
  TAlphaColors = System.UITypes.TAlphaColors;

  TPointF = Types.TPointF;
  TRectF = Types.TRectF;

  TList_TPointF = specialize TArrayList<TPointF>;
  TList_TRectF = specialize TArrayList<TRectF>;

  TArr_TPointF = array of TPointF;
  TArr_TRectF = array of TRectF;

  TArr_TSDL_Color = array of TSDL_Color;
  TArr_TSDL_FRect = array of TSDL_FRect;

type
  TSDL_FPointHelper = type Helper for TSDL_FPoint
  public
    function ToPtr: PSDL_FPoint;
  end;

  TSDL_FRectHelper = type helper for TSDL_FRect
  public
    function ToPtr: PSDL_FRect;
  end;

  TSDL_ColorHelper = type helper for TSDL_Color
  public
    function ToPtr: PSDL_Color;
  end;

operator := (AColor: TColor): TSDL_Color;
operator := (AColor: TAlphaColor): TSDL_Color;
operator := (AColor: TColor): TColors;
operator := (AColor: TAlphaColor): TAlphaColors;

function SDL_FPoint(x, y: float): TSDL_FPoint;
function SDL_FPoint(p: TPoint): TSDL_FPoint;

function SDL_FRect(x, y, w, h: float): TSDL_FRect;
function SDL_FRect(rect: TRect): TSDL_FRect;

function SDL_Color(r, g, b, a: byte): TSDL_Color;
function SDL_ColorF(r, g, b, a: single): TSDL_FColor;


implementation

operator := (AColor: TColor): TSDL_Color;
var
  temp: TColors;
  res: TSDL_Color;
begin
  temp := TColors(AColor);
  res := Default(TSDL_Color);

  res.r := temp.R;
  res.g := temp.G;
  res.b := temp.B;
  res.a := $FF;

  Result := res;
end;

operator := (AColor: TAlphaColor): TSDL_Color;
var
  temp: TAlphaColors;
  res: TSDL_Color;
begin
  temp := TAlphaColors(AColor);
  res := Default(TSDL_Color);

  res.r := temp.R;
  res.g := temp.G;
  res.b := temp.B;
  res.a := temp.A;

  Result := res;
end;

operator := (AColor: TColor): TColors;
begin
  Result := AColor;
end;

operator := (AColor: TAlphaColor): TAlphaColors;
begin
  Result := TAlphaColors.Create(AColor);
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

function SDL_FRect(rect: TRect): TSDL_FRect;
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

end.

