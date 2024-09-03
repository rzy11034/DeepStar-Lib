unit DeepStar.OpenGL.Utils;

{$mode ObjFPC}{$H+}
{$ModeSwitch unicodestrings}{$J-}
{$ModeSwitch advancedrecords}{$J-}
{$modeswitch typehelpers}

interface

uses
  Classes,
  SysUtils,
  System.UITypes,
  DeepStar.Utils,
  DeepStar.DSA.Linear.ArrayList,
  DeepStar.OpenGL.GLM,
  DeepStar.OpenGL.GLAD_GL;

const
  LE = LineEnding;

  SIZE_OF_F = Sizeof(GLfloat);
  SIZE_OF_I = SizeOf(GLint);

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

  TArrayList_TVec3 = specialize TArrayList<TVec3>;
  TArrayList_TVec4 = specialize TArrayList<TVec4>;
  TArrayList_TMat4 = specialize TArrayList<TMat4>;

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


function OpenGLColor(red, green, blue: GLubyte): TVec3;
function RGBToOpenGLColor(c: TAlphaColor): TVec3;

function OpenGLColorAlpha(red, green, blue: GLubyte; alpha: GLubyte = $FF): TVec4;
function RGBAToOpenGLColor(c: TAlphaColor): TVec4;

implementation

function OpenGLColor(red, green, blue: GLubyte): TVec3;
begin
  Result.r := red / 255;
  Result.g := green / 255;
  Result.b := blue / 255;
end;

function RGBToOpenGLColor(c: TAlphaColor): TVec3;
var
  temp: TAlphaColors;
begin
  temp := TAlphaColors.Create(c);
  Result := OpenGLColor(temp.R, temp.G, temp.B);
end;

function OpenGLColorAlpha(red, green, blue: GLubyte; alpha: GLubyte): TVec4;
begin
  Result.r := red / 255;
  Result.g := green / 255;
  Result.b := blue / 255;
  Result.a := alpha / 255;
end;

function RGBAToOpenGLColor(c: TAlphaColor): TVec4;
var
  temp: TAlphaColors;
begin
  temp := TAlphaColors.Create(c);
  Result := OpenGLColorAlpha(temp.R, temp.G, temp.B, temp.A);
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
