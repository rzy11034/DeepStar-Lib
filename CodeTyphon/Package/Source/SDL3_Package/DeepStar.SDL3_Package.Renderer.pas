unit DeepStar.SDL3_Package.Renderer;

{$mode ObjFPC}{$H+}
{$ModeSwitch unicodestrings}{$J-}

interface

uses
  Classes,
  SysUtils,
  libSDL3,
  DeepStar.Utils,
  DeepStar.SDL3_Package.Utils,
  DeepStar.SDL3_Package.Texture;

type
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
    constructor Create(win: PSDL_Window; name: string);
    destructor Destroy; override;

    procedure Clear;

    procedure SetDrawColor(color: TSDL_Color);
    procedure SetDrawColor(r, g, b, a: Byte);

    procedure SetDrawColorFloat(color: TSDL_FColor);
    procedure SetDrawColorFloat(r, g, b, a: Single);

    // 渲染
    procedure Render(texture: TTexture);
    procedure Render(texture: TTexture; srcRect: PSDL_FRect; destRect: PSDL_FRect);
    procedure Render(texture: TTexture; p: TPointF);

    procedure RenderPresent;
  end;

implementation

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
begin
  sdl_render
end;

procedure TRenderer.Render(texture: TTexture; srcRect: PSDL_FRect; destRect: PSDL_FRect);
begin

end;

procedure TRenderer.Render(texture: TTexture; p: TPointF);
begin

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

end.

