unit DeepStar.OpenGL.Texture;

{$mode ObjFPC}{$H+}
{$ModeSwitch unicodestrings}{$J-}

interface

uses
  Classes,
  SysUtils,
  Math,
  System.UITypes,
  FpImage,
  FPReadJPEG,
  FPReadPNG,
  DeepStar.Utils,
  DeepStar.OpenGL.GLAD_GL;

type
  TTexture = class(TInterfacedObject)
  private
    _pixels: array of GLuint;
    _width: GLint;
    _height: GLint;
    _inverse: boolean;
    _useAlpha: boolean;

    function __FPColorToAlphaColor(const Value: TFPColor): GLuint;
    function __GetDate: Pointer;
    function __GetHeight: GLint;
    function __GetInverse: Boolean;
    function __GetUseAlpha: boolean;
    function __GetWidth: GLint;
    procedure __SetInverse(const value: Boolean);

  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFormFile(const fileName: string;  inverse: boolean = true);

    property Width: GLint read __GetWidth;
    property Height: GLint read __GetHeight;
    property Pixels: Pointer read __GetDate;
    property UseAlpha: boolean read __GetUseAlpha;
    property Inverse: Boolean read __GetInverse write __SetInverse;
  end;

implementation

{ TTexture }

constructor TTexture.Create;
begin
  inherited Create;
end;

destructor TTexture.Destroy;
begin
  inherited Destroy;
end;

procedure TTexture.LoadFormFile(const fileName: string; inverse: boolean);
var
  img: TFPMemoryImage;
  readerClass: TFPCustomImageReaderClass;
  reader: TFPCustomImageReader;
  c: TFPColor;
  y, x, k: integer;
  fn: AnsiString;
begin
  fn := CrossFixFileName(fileName).ToAnsiString;

  img := TFPMemoryImage.Create(0, 0);
  readerClass := img.FindReaderFromFileName(fn);

  reader := readerClass.Create;
  try
    img.LoadFromFile(fn, reader);

    if reader is TFPReaderPNG then
      _useAlpha := (reader as TFPReaderPNG).UseAlpha;

    _width := img.Width;
    _height := img.Height;

    SetLength(_pixels, Width * Height);
    k := 0;

    if inverse then
    begin
      for y := Height - 1 downto 0 do
        for x := 0 to Width - 1 do
        begin
          c := img.Colors[x, y];

          _pixels[k] := __FPColorToAlphaColor(c);
          k += 1;
        end;
    end
    else
    begin
      for y := 0 to Height - 1 do
        for x := 0 to Width - 1 do
        begin
          c := img.Colors[x, y];

          _pixels[k] := __FPColorToAlphaColor(c);
          k += 1;
        end;
    end;
  finally
    reader.Free;
    img.Free;
  end;
end;

function TTexture.__FPColorToAlphaColor(const Value: TFPColor): GLuint;
var
  ac: TColors;
begin
  ac := Default(TColors);

  ac.A := Value.Alpha div 2 ** 8;
  ac.R := Value.Red div 2 ** 8;
  ac.G := Value.Green div 2 ** 8;
  ac.B := Value.Blue div 2 ** 8;

  Result := ac.Color;
end;

function TTexture.__GetDate: Pointer;
begin
  Result := @_pixels[0];
end;

function TTexture.__GetHeight: GLint;
begin
  Result := _height;
end;

function TTexture.__GetInverse: Boolean;
begin
  Result := _inverse;
end;

function TTexture.__GetUseAlpha: boolean;
begin
  Result := _useAlpha;
end;

function TTexture.__GetWidth: GLint;
begin
  Result := _width;
end;

procedure TTexture.__SetInverse(const value: Boolean);
var
  l, r: integer;
  temp: GLuint;
begin
  if value = _inverse then Exit;

  _inverse := value;

  l := 0;
  r := High(_pixels);

  while l < r do
  begin
    temp := _pixels[l];
    _pixels[l] := _pixels[r];
    _pixels[r] := temp;

    l += 1;
    r -= 1;
  end;
end;

end.
