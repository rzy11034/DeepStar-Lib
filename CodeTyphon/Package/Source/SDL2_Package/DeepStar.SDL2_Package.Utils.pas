unit DeepStar.SDL2_Package.Utils;

{$mode ObjFPC}{$H+}
{$ModeSwitch unicodestrings}{$J-}
{$ModeSwitch advancedrecords}{$J-}
{$modeswitch typehelpers}

interface

uses
  Classes,
  SysUtils,
  Types,
  libSDL2,
  System.UITypes,
  DeepStar.Utils,
  DeepStar.DSA.Linear.ArrayList;

type
  float = single;

  TColor = System.UITypes.TColor;
  TColors = System.UITypes.TColors;
  TAlphaColor = System.UITypes.TAlphaColor;
  TAlphaColors = System.UITypes.TAlphaColors;

  TList_TPoint = specialize TArrayList<TPoint>;
  TList_TRect = specialize TArrayList<TRect>;

  TArr_TPoint = array of TPoint;
  TArr_TRect = array of TRect;

  TArr_TSDL_Color = array of TSDL_Color;
  TArr_TSDL_Rect = array of TSDL_Rect;

type
  TSDL_PointHelper = type Helper for TSDL_Point
  public
    function ToPtr: PSDL_Point;
  end;

  TSDL_RectHelper = type helper for TSDL_Rect
  public
    function ToPtr: PSDL_Rect;
  end;

  TSDL_ColorHelper = type helper for TSDL_Color
  public
    function ToPtr: PSDL_Color;
  end;

var
  SDL_BlitSurface: TTSDL_UpperBlit;

operator := (AColor: TColor): TSDL_Color;
operator := (AColor: TAlphaColor): TSDL_Color;
operator := (AColor: TColor): TColors;
operator := (AColor: TAlphaColor): TAlphaColors;

function SDL_Point(aX, aY: integer): TSDL_Point;
function SDL_Point(p: TPoint): TSDL_Point;

function SDL_Rect(aX, aY, aW, aH: integer): TSDL_Rect;
function SDL_Rect(rc: TRect): TSDL_Rect;

function SDL_Color(r, g, b, a: byte): TSDL_Color;

procedure CostomLibarayLoad;

implementation

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

function SDL_Point(aX, aY: integer): TSDL_Point;
var
  res: TSDL_Point;
begin
  res := Default(TSDL_Point);

  with res do
  begin
    x := aX;
    y := aY;
  end;

  Result := res;
end;

function SDL_Point(p: TPoint): TSDL_Point;
begin
  Result := SDL_Point(p.X, p.Y);
end;

function SDL_Rect(aX, aY, aW, aH: integer): TSDL_Rect;
var
  res: TSDL_Rect;
begin
  res := Default(TSDL_Rect);

  res.x := aX;
  res.y := aY;
  res.w := aW;
  res.h := aH;

  Result := res;
end;

function SDL_Rect(rc: TRect): TSDL_Rect;
var
  res: TSDL_Rect;
begin rc.NormalizeRect;
  res := Default(TSDL_Rect);

  with res do
  begin
    x := rc.Left;
    y := rc.Top;
    w := rc.Width;
    h := rc.Height;
  end;

  Result := res;
end;

function SDL_Color(r, g, b, a: byte): TSDL_Color;
begin
  Result.r := r;
  Result.g := g;
  Result.b := b;
  Result.a := a;
end;

procedure CostomLibarayLoad;
begin
  SDL_BlitSurface := SDL_UpperBlit;
end;

{ TSDL_PointHelper }

function TSDL_PointHelper.ToPtr: PSDL_Point;
begin
  Result := @Self;
end;

{ TSDL_RectHelper }

function TSDL_RectHelper.ToPtr: PSDL_Rect;
begin
  Result := @Self;
end;

{ TSDL_ColorHelper }

function TSDL_ColorHelper.ToPtr: PSDL_Color;
begin
  Result := @Self;
end;

end.
