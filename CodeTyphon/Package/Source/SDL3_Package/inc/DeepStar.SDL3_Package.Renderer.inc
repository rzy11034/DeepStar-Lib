type
  TTexture = Class;

  TRenderer = class(TInterfacedObject)
  private
    _Renderer: PSDL_Renderer;
    _Color: TSDL_Color;
    _ColorF: TSDL_FColor;

    procedure __SetDrawColor;
    procedure __GetDrawColor;

    procedure __SetDrawColorF;
    procedure __GetDrawColorF;

  public
    constructor Create(win: PSDL_Window; name: string = '');
    destructor Destroy; override;

    function ToPSDL_Renderer: PSDL_Renderer;

    procedure Clear;

    procedure SetDrawColor(color: TSDL_Color);
    procedure SetDrawColor(r, g, b, a: Byte);

    procedure SetDrawColorFloat(color: TSDL_FColor);
    procedure SetDrawColorFloat(r, g, b, a: Single);

    // 渲染
    procedure Render(texture: TTexture);
    procedure Render(texture: TTexture; p: TPointF);
    procedure Render(texture: TTexture; srcRect: PSDL_FRect; destRect: PSDL_FRect);
    procedure Render(texture: TTexture; x, y: Single; clip: PSDL_FRect;
                angle: double; center: PSDL_FPoint; flip: TSDL_FlipMode);

    procedure RenderPresent;
  end;

