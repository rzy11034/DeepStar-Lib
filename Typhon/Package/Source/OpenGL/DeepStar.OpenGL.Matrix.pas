unit DeepStar.OpenGL.Matrix;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}{$J-}

interface

uses
  Classes,
  SysUtils,
  DeepStar.OpenGL.GLAD_GL,
  DeepStar.OpenGL.Vector;

type
  TMat4 = packed record
    constructor Create(x00, x01, x02, x03, x10, x11, x12, x13, x20, x21, x22, x23, x30, x31, x32, x33: GLfloat);
    procedure InitZero;
    procedure InitIdentity;
    function GetColumn(c: byte): Tvec4;
    function GetRow(r: byte): TVec4;
    //function determinant: GLfloat;
    //function inverse(Adeterminant: single): TMat4;
    function Transpose: TMat4;

    case integer of
      0: (m00, m01, m02, m03, m10, m11, m12, m13, m20, m21, m22, m23, m30, m31, m32, m33: GLfloat);
      1: (Data: array[0..3, 0..3] of GLfloat);
  end;


implementation

{ TMat4 }

constructor TMat4.Create(x00, x01, x02, x03, x10, x11, x12, x13, x20, x21, x22,
  x23, x30, x31, x32, x33: GLfloat);
begin
  // column 1
  m00 := x00;
  m01 := x01;
  m02 := x02;
  m03 := x03;

  // column 2
  m10 := x10;
  m11 := x11;
  m12 := x12;
  m13 := x13;

  //column 3
  m20 := x20;
  m21 := x21;
  m22 := x22;
  m23 := x23;

  // column
  m30 := x30;
  m31 := x31;
  m32 := x32;
  m33 := x33;
end;

function TMat4.GetColumn(c: byte): Tvec4;
var
  res: TVec4;
begin
  case c of
    0: res := TVec4.Create(m00, m01, m02, m03);
    1: res := TVec4.Create(m10, m11, m12, m13);
    2: res := TVec4.Create(m20, m21, m22, m23);
    3: res := TVec4.Create(m30, m31, m32, m33);
  end;
  Result := res;
end;

function TMat4.GetRow(r: byte): TVec4;
var
  res: TVec4;
begin
  case r of
    0: res := TVec4.Create(m00, m10, m20, m30);
    1: res := TVec4.Create(m01, m11, m21, m31);
    2: res := TVec4.Create(m02, m11, m22, m32);
    3: res := TVec4.Create(m03, m11, m23, m33);
  end;
  Result := res;
end;

procedure TMat4.InitIdentity;
begin
  Create(
    1, 0, 0, 0,
    0, 1, 0, 0,
    0, 0, 1, 0,
    0, 0, 0, 1);
end;

procedure TMat4.InitZero;
begin
  Create(
    0, 0, 0, 0,
    0, 0, 0, 0,
    0, 0, 0, 0,
    0, 0, 0, 0);
end;

function TMat4.Transpose: TMat4;
begin
  Result := Create(
    m00, m10, m20, m30,
    m01, m11, m21, m31,
    m02, m12, m22, m32,
    m03, m13, m23, m33);
end;

end.
