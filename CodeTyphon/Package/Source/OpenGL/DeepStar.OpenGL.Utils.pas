unit DeepStar.OpenGL.Utils;

{$mode ObjFPC}{$H+}
{$ModeSwitch unicodestrings}{$J-}
{$ModeSwitch advancedrecords}{$J-}
{$modeswitch typehelpers}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils,
  DeepStar.OpenGL.GLM,
  DeepStar.OpenGL.GLAD_GL;

const
  LE = LineEnding;

type
  TArr_GLboolean = array of GLboolean;
  TArr_GLfloat = array of GLfloat;
  TArr_GLint = array of GLint;
  TArr_GLuint = array of GLuint;
  TArr_GLchar = array of GLchar;
  TArr_GLdouble = array of GLdouble;

  TArr_TVec2 = array of TVec2;
  TArr_TVec3 = array of TVec3;
  TArr_TVec4 = array of TVec4;
  TArr_TMat3 = array of TMat3;
  TArr_TMat4 = array of TMat4;

  TArr_GLfloat4 = array[0..3] of GLfloat;
  TArr_GLfloat16 = array[0..15] of GLfloat;

type
  TArr_GLfloat_Helper = type Helper for TArr_GLfloat
  private type
    TArrayUtils_GLfloat = specialize TArrayUtils<GLfloat>;
  public
    // 返回数组内存区占用大小
    function MemSize: integer;
  end;

  TArr_GLint_Helper = type Helper for TArr_GLint
  private type
    TArrayUtils_GLint = specialize TArrayUtils<GLint>;
  public
    // 返回数组内存区占用大小
    function MemSize: integer;
  end;


function RGBAToOpenGLColor(red, green, blue: GLubyte; alpha: GLubyte = 0): TArr_GLfloat4;
function HtmlRGBToOpenGLColor(HtmlColor: GLuint): TArr_GLfloat4;

implementation

function RGBAToOpenGLColor(red, green, blue: GLubyte; alpha: GLubyte): TArr_GLfloat4;
var
  res: TArr_GLfloat4;
begin
  res[0] := red / 255;
  res[1] := green / 255;
  res[2] := blue / 255;
  res[3] := alpha / 255;
  Result := res;
end;

function HtmlRGBToOpenGLColor(HtmlColor: GLuint): TArr_GLfloat4;
var
  r, g, b, a: GLubyte;
  p: PGLubyte;
begin
  p := @HtmlColor;
  b := p[0];
  g := p[1]; //Inc(p);
  r := p[2]; //Inc(p);
  a := p[3];

  Result := RGBAToOpenGLColor(r, g, b, a);
end;

{ TArr_GLfloat_Helper }

function TArr_GLfloat_Helper.MemSize: integer;
begin
  Result := TArrayUtils_GLfloat.MemorySize(Self);
end;

{ TArr_GLint_Helper }

function TArr_GLint_Helper.MemSize: integer;
begin
  Result := TArrayUtils_GLint.MemorySize(Self);
end;

end.
