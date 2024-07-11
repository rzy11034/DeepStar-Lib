unit DeepStar.OpenGL.Texture;

{$mode ObjFPC}{$H+}
{$ModeSwitch unicodestrings}{$J-}

interface

uses
  Classes,
  SysUtils,
  FpImage,
  FPReadJPEG,
  FPReadPNG,
  DeepStar.Utils,
  DeepStar.OpenGL.GLAD_GL;

type
  TTexture = class(TObject)
  private
    _Pixels: array of GLuint;
    _Width: GLint;
    _Height: GLint;

    function __GetDate: Pointer;
    function __FPColorToColor(const Value: TFPColor): GLuint;
    function __GetHeight: GLint;
    function __GetWidth: GLint;

  public
    constructor Create(const fileName: string);
    destructor Destroy; override;

    property Width: GLint read __GetWidth;
    property Height: GLint read __GetHeight;
    property Pixels: Pointer read __GetDate;
  end;

implementation

{ TTexture }

constructor TTexture.Create(const fileName: string);
var
  img: TFPMemoryImage;
  readerClass: TFPCustomImageReaderClass;
  reader: TFPCustomImageReader;
  y: GLint;
  x: integer;
  c: TFPColor;
begin
  inherited Create;

  img := TFPMemoryImage.Create(0, 0);
  readerClass := img.FindReaderFromFileName(fileName.ToAnsiString);
  reader := readerClass.Create;

  try
    img.LoadFromFile(fileName.ToAnsiString, reader);

    _Width := img.Width;
    _Height := img.Height;

    SetLength(_Pixels, Width * Height);

    for y := Height - 1 downto 0 do
      for x := 0 to Width - 1 do
      begin
        c := img.Colors[x, y];

        _Pixels[x + ((Height - y - 1) * Width)] := __FPColorToColor(c);
      end;
  finally
    reader.Free;
    img.Free;
  end;
end;

destructor TTexture.Destroy;
begin
  inherited Destroy;
end;

function TTexture.__FPColorToColor(const Value: TFPColor): GLuint;
begin
  Result := ((Value.Red shr 8) and $FF)
    or (Value.Green and $FF00)
    or ((Value.Blue shl 8) and $FF0000);
end;

function TTexture.__GetDate: Pointer;
begin
  Result := @_Pixels[0];
end;

function TTexture.__GetHeight: GLint;
begin
  Result := _Height;
end;

function TTexture.__GetWidth: GLint;
begin
  Result := _Width;
end;

end.
