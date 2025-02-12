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
  TSDL_FPointHelper = type helper for TSDL_FPoint
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

function PointF(x, y: Single): TPointF;
function RectF(p: TPointF; w, h: Single): TRectF;

function SDL_FPoint(x, y: float): TSDL_FPoint;
function SDL_FPoint(p: TPoint): TSDL_FPoint;

function SDL_FRect(x, y, w, h: float): TSDL_FRect;
function SDL_FRect(rect: TRectF): TSDL_FRect;

function SDL_Color(r, g, b, a: byte): TSDL_Color;
function SDL_ColorF(r, g, b, a: single): TSDL_FColor;

