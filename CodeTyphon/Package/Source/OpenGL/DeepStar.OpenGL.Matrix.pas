unit DeepStar.OpenGL.Matrix;

{$mode objfpc}{$H+}
{$ModeSwitch unicodestrings}{$J-}
{$ModeSwitch advancedrecords}

interface

uses
  Classes,
  SysUtils,
  DeepStar.OpenGL.Vector;

type
  TMat3 = packed record
    constructor Create(x00, x01, x02, x10, x11, x12, x20, x21, x22: single);
    procedure Init_Zero;
    procedure Init_Identity;
    function GetColumn(index: byte): TVec3;
    function GetRow(index: byte): TVec3;
    procedure SetColumn(index: byte; const vec: TVec3);
    procedure SetRow(index: byte; const vec: TVec3);
    function GetDeterminant: single;
    function inverse(determinant: single): TMat3;
    function Transpose: TMat3;
    class operator +(const mat1, mat2: TMat3): TMat3;
    class operator +(const mat: TMat3; x: single): TMat3;
    class operator -(const mat1, mat2: TMat3): TMat3;
    class operator -(const mat: TMat3): TMat3;
    class operator -(const mat: TMat3; x: single): TMat3;
    class operator * (const mat1, mat2: TMat3): TMat3;
    class operator * (const mat: TMat3; v: TVec3): TVec3;
    class operator * (const mat: TMat3; x: single): TMat3;
    class operator / (const mat: TMat3; x: single): TMat3;

    case integer of
      0: (m: array[0..2, 0..2] of single);
      1: (v: array[0..2] of TVec3);
      2: (m00, m01, m02,
          m10, m11, m12,
          m20, m21, m22: single);
  end;

  TMat4 = packed record
    constructor Create(x00, x01, x02, x03, x10, x11, x12, x13, x20, x21, x22, x23,
      x30, x31, x32, x33: single);
    procedure Init_Zero;
    procedure Init_Identity;
    function GetColumn(index: byte): TVec4;
    function GetRow(index: byte): TVec4;
    procedure SetColumn(index: byte; const vec: TVec4);
    procedure SetRow(index: byte; const vec: TVec4);
    function GetDeterminant: single;
    function inverse(determinant: single): TMat4;
    function Transpose: TMat4;
    class operator +(const mat1, mat2: TMat4): TMat4;
    class operator +(const mat: TMat4; x: single): TMat4;
    class operator -(const mat1, mat2: TMat4): TMat4;
    class operator -(const mat: TMat4): TMat4;
    class operator -(const mat: TMat4; x: single): TMat4;
    class operator * (const mat1, mat2: TMat4): TMat4;
    class operator * (const mat: TMat4; v: TVec4): TVec4;
    class operator * (const mat: TMat4; x: single): TMat4;
    class operator / (const mat: TMat4; x: single): TMat4;

    case integer of
      0: (m: array[0..3, 0..3] of single);
      1: (v: array[0..3] of TVec4);
      1: (m00, m01, m02, m03,
          m10, m11, m12, m13,
          m20, m21, m22, m23,
          m30, m31, m32, m33: single);
  end;


implementation

const
  ERROR_3X3 = 'index is not in [0..2]';
  ERROR_4X4 = 'index is not in [0..3]';

  { TMat3 }

class operator TMat3.*(const mat1, mat2: TMat3): TMat3;
var
  i, j, k: integer;
begin
  for i := 0 to 2 do
  begin
    for j := 0 to 2 do
    begin
      Result.m[i, j] := 0.0;
      for k := 0 to 2 do
      begin
        Result.m[i, j] += mat2.m[i, k] * mat1.m[k, j];
      end;
    end;
  end;
end;

class operator TMat3. * (const mat: TMat3; x: single): TMat3;
begin
  Result.m[0, 0] := mat.m[0, 0] * x;
  Result.m[0, 1] := mat.m[0, 1] * x;
  Result.m[0, 2] := mat.m[0, 2] * x;
  Result.m[1, 0] := mat.m[1, 0] * x;
  Result.m[1, 1] := mat.m[1, 1] * x;
  Result.m[1, 2] := mat.m[1, 2] * x;
  Result.m[2, 0] := mat.m[2, 0] * x;
  Result.m[2, 1] := mat.m[2, 1] * x;
  Result.m[2, 2] := mat.m[2, 2] * x;
end;

class operator TMat3. * (const mat: TMat3; v: TVec3): TVec3;
begin
  Result.v[0] := mat.m[0, 0] * v.v[0] + mat.m[0, 1] * v.v[1] + mat.m[0, 2] * v.v[2];
  Result.v[1] := mat.m[1, 0] * v.v[0] + mat.m[1, 1] * v.v[1] + mat.m[1, 2] * v.v[2];
  Result.v[2] := mat.m[2, 0] * v.v[0] + mat.m[2, 1] * v.v[1] + mat.m[2, 2] * v.v[2];
end;

class operator TMat3. +(const mat1, mat2: TMat3): TMat3;
begin
  Result.m[0, 0] := mat1.m[0, 0] + mat2.m[0, 0];
  Result.m[0, 1] := mat1.m[0, 1] + mat2.m[0, 1];
  Result.m[0, 2] := mat1.m[0, 2] + mat2.m[0, 2];
  Result.m[1, 0] := mat1.m[1, 0] + mat2.m[1, 0];
  Result.m[1, 1] := mat1.m[1, 1] + mat2.m[1, 1];
  Result.m[1, 2] := mat1.m[1, 2] + mat2.m[1, 2];
  Result.m[2, 0] := mat1.m[2, 0] + mat2.m[2, 0];
  Result.m[2, 1] := mat1.m[2, 1] + mat2.m[2, 1];
  Result.m[2, 2] := mat1.m[2, 2] + mat2.m[2, 2];
end;

class operator TMat3. +(const mat: TMat3; x: single): TMat3;
begin
  Result.m[0, 0] := mat.m[0, 0] + x;
  Result.m[0, 1] := mat.m[0, 1] + x;
  Result.m[0, 2] := mat.m[0, 2] + x;
  Result.m[1, 0] := mat.m[1, 0] + x;
  Result.m[1, 1] := mat.m[1, 1] + x;
  Result.m[1, 2] := mat.m[1, 2] + x;
  Result.m[2, 0] := mat.m[2, 0] + x;
  Result.m[2, 1] := mat.m[2, 1] + x;
  Result.m[2, 2] := mat.m[2, 2] + x;
end;

class operator TMat3. -(const mat1, mat2: TMat3): TMat3;
begin
  Result.m[0, 0] := mat1.m[0, 0] - mat2.m[0, 0];
  Result.m[0, 1] := mat1.m[0, 1] - mat2.m[0, 1];
  Result.m[0, 2] := mat1.m[0, 2] - mat2.m[0, 2];
  Result.m[1, 0] := mat1.m[1, 0] - mat2.m[1, 0];
  Result.m[1, 1] := mat1.m[1, 1] - mat2.m[1, 1];
  Result.m[1, 2] := mat1.m[1, 2] - mat2.m[1, 2];
  Result.m[2, 0] := mat1.m[2, 0] - mat2.m[2, 0];
  Result.m[2, 1] := mat1.m[2, 1] - mat2.m[2, 1];
  Result.m[2, 2] := mat1.m[2, 2] - mat2.m[2, 2];
end;

class operator TMat3. -(const mat: TMat3): TMat3;
begin
  Result.m[0, 0] := -mat.m[0, 0];
  Result.m[0, 1] := -mat.m[0, 1];
  Result.m[0, 2] := -mat.m[0, 2];
  Result.m[1, 0] := -mat.m[1, 0];
  Result.m[1, 1] := -mat.m[1, 1];
  Result.m[1, 2] := -mat.m[1, 2];
  Result.m[2, 0] := -mat.m[2, 0];
  Result.m[2, 1] := -mat.m[2, 1];
  Result.m[2, 2] := -mat.m[2, 2];
end;

class operator TMat3. -(const mat: TMat3; x: single): TMat3;
begin
  Result.m[0, 0] := mat.m[0, 0] - x;
  Result.m[0, 1] := mat.m[0, 1] - x;
  Result.m[0, 2] := mat.m[0, 2] - x;
  Result.m[1, 0] := mat.m[1, 0] - x;
  Result.m[1, 1] := mat.m[1, 1] - x;
  Result.m[1, 2] := mat.m[1, 2] - x;
  Result.m[2, 0] := mat.m[2, 0] - x;
  Result.m[2, 1] := mat.m[2, 1] - x;
  Result.m[2, 2] := mat.m[2, 2] - x;
end;

class operator TMat3. / (const mat: TMat3; x: single): TMat3;
begin
  Result.m[0, 0] := mat.m[0, 0] / x;
  Result.m[0, 1] := mat.m[0, 1] / x;
  Result.m[0, 2] := mat.m[0, 2] / x;
  Result.m[1, 0] := mat.m[1, 0] / x;
  Result.m[1, 1] := mat.m[1, 1] / x;
  Result.m[1, 2] := mat.m[1, 2] / x;
  Result.m[2, 0] := mat.m[2, 0] / x;
  Result.m[2, 1] := mat.m[2, 1] / x;
  Result.m[2, 2] := mat.m[2, 2] / x;
end;

constructor TMat3.Create(x00, x01, x02, x10, x11, x12, x20, x21, x22: single);
begin
  m00 := x00; m01 := x01; m02 := x02;
  m10 := x10; m11 := x11; m12 := x12;
  m20 := x20; m21 := x21; m22 := x22;
end;

function TMat3.GetColumn(index: byte): TVec3;
var
  res: TVec3;
begin
  case index of
    0: res := TVec3.Create(m00, m01, m02);
    1: res := TVec3.Create(m10, m11, m12);
    2: res := TVec3.Create(m20, m21, m22);
    else
      raise Exception.Create(ERROR_3X3);
  end;
  Result := res;
end;

function TMat3.GetDeterminant: single;
begin
  Result :=
    m[0, 0] * (m[1, 1] * m[2, 2] - m[1, 2] * m[2, 1]) -
    m[0, 1] * (m[1, 0] * m[2, 2] - m[1, 2] * m[2, 0]) +
    m[0, 2] * (m[1, 0] * m[2, 1] - m[1, 1] * m[2, 0]);
end;

function TMat3.GetRow(index: byte): TVec3;
var
  res: TVec3;
begin
  case index of
    0: res := TVec3.Create(m00, m10, m20);
    1: res := TVec3.Create(m01, m11, m21);
    2: res := TVec3.Create(m02, m11, m22);
    else
      raise Exception.Create(ERROR_3X3);
  end;
  Result := res;
end;

procedure TMat3.Init_Identity;
begin
  Self.m := [
    [1, 0, 0],
    [0, 1, 0],
    [0, 0, 1]];
end;

procedure TMat3.Init_Zero;
begin
  Self.m := [
    [0, 0, 0],
    [0, 0, 0],
    [0, 0, 0]];
end;

function TMat3.inverse(determinant: single): TMat3;
begin
  determinant := 1 / determinant;
  Result.m[0, 0] := (m[1, 1] * m[2, 2] - m[2, 1] * m[1, 2]) * determinant;
  Result.m[0, 1] := -(m[0, 1] * m[2, 2] - m[2, 1] * m[0, 2]) * determinant;
  Result.m[0, 2] := (m[0, 1] * m[1, 2] - m[1, 1] * m[0, 2]) * determinant;
  Result.m[1, 0] := -(m[1, 0] * m[2, 2] - m[2, 0] * m[1, 2]) * determinant;
  Result.m[1, 1] := (m[0, 0] * m[2, 2] - m[2, 0] * m[0, 2]) * determinant;
  Result.m[1, 2] := -(m[0, 0] * m[1, 2] - m[1, 0] * m[0, 2]) * determinant;
  Result.m[2, 0] := (m[1, 0] * m[2, 1] - m[2, 0] * m[1, 1]) * determinant;
  Result.m[2, 1] := -(m[0, 0] * m[2, 1] - m[2, 0] * m[0, 1]) * determinant;
  Result.m[2, 2] := (m[0, 0] * m[1, 1] - m[1, 0] * m[0, 1]) * determinant;
end;

procedure TMat3.SetColumn(index: byte; const vec: TVec3);
begin
  case index of
    0: begin
      m00 := vec.x; m10 := vec.y; m20 := vec.z;
    end;

    1: begin
      m01 := vec.x; m11 := vec.y; m21 := vec.z;
    end;

    2: begin
      m02 := vec.x; m12 := vec.y; m22 := vec.z;
    end;

    else
      raise Exception.Create(ERROR_3X3); ;
  end;
end;

procedure TMat3.SetRow(index: byte; const vec: TVec3);
begin
  case index of
    0: begin
      m00 := vec.x; m01 := vec.y; m02 := vec.z;
    end;

    1: begin
      m10 := vec.x; m11 := vec.y; m12 := vec.z;
    end;

    2: begin
      m20 := vec.x; m21 := vec.y; m22 := vec.z;
    end;

    else
      raise Exception.Create(ERROR_3X3); ;
  end;
end;

function TMat3.Transpose: TMat3;
begin
  Result.Create(
    m00, m10, m20,
    m01, m11, m21,
    m02, m12, m22);
end;


{ TMat4 }

class operator TMat4.*(const mat1, mat2: TMat4): TMat4;
var
  i, j, k: integer;
begin
  for i := 0 to 3 do
  begin
    for j := 0 to 3 do
    begin
      Result.m[i, j] := 0.0;
      for k := 0 to 3 do
      begin
        Result.m[i, j] += mat2.m[i, k] * mat1.m[k, j];
      end;
    end;
  end;
end;

class operator TMat4. * (const mat: TMat4; x: single): TMat4;
begin
  Result.m[0, 0] := mat.m[0, 0] * x;
  Result.m[0, 1] := mat.m[0, 1] * x;
  Result.m[0, 2] := mat.m[0, 2] * x;
  Result.m[0, 3] := mat.m[0, 3] * x;
  Result.m[1, 0] := mat.m[1, 0] * x;
  Result.m[1, 1] := mat.m[1, 1] * x;
  Result.m[1, 2] := mat.m[1, 2] * x;
  Result.m[1, 3] := mat.m[1, 3] * x;
  Result.m[2, 0] := mat.m[2, 0] * x;
  Result.m[2, 1] := mat.m[2, 1] * x;
  Result.m[2, 2] := mat.m[2, 2] * x;
  Result.m[2, 3] := mat.m[2, 3] * x;
  Result.m[3, 0] := mat.m[3, 0] * x;
  Result.m[3, 1] := mat.m[3, 1] * x;
  Result.m[3, 2] := mat.m[3, 2] * x;
  Result.m[3, 3] := mat.m[3, 3] * x;
end;

class operator TMat4. * (const mat: TMat4; v: TVec4): TVec4;
begin
  Result.v[0] := mat.m[0, 0] * v.v[0] + mat.m[0, 1] * v.v[1]
    + mat.m[0, 2] * v.v[2] + mat.m[0, 3] * v.v[3];

  Result.v[1] := mat.m[1, 0] * v.v[0] + mat.m[1, 1] * v.v[1]
    + mat.m[1, 2] * v.v[2] + mat.m[1, 3] * v.v[3];

  Result.v[2] := mat.m[2, 0] * v.v[0] + mat.m[2, 1] * v.v[1]
    + mat.m[2, 2] * v.v[2] + mat.m[2, 3] * v.v[3];

  Result.v[3] := mat.m[3, 0] * v.v[0] + mat.m[3, 1] * v.v[1]
    + mat.m[3, 2] * v.v[2] + mat.m[3, 3] * v.v[3];
end;

class operator TMat4. +(const mat1, mat2: TMat4): TMat4;
begin
  Result.m[0, 0] := mat1.m[0, 0] + mat2.m[0, 0];
  Result.m[0, 1] := mat1.m[0, 1] + mat2.m[0, 1];
  Result.m[0, 2] := mat1.m[0, 2] + mat2.m[0, 2];
  Result.m[0, 3] := mat1.m[0, 3] + mat2.m[0, 3];
  Result.m[1, 0] := mat1.m[1, 0] + mat2.m[1, 0];
  Result.m[1, 1] := mat1.m[1, 1] + mat2.m[1, 1];
  Result.m[1, 2] := mat1.m[1, 2] + mat2.m[1, 2];
  Result.m[1, 3] := mat1.m[1, 3] + mat2.m[1, 3];
  Result.m[2, 0] := mat1.m[2, 0] + mat2.m[2, 0];
  Result.m[2, 1] := mat1.m[2, 1] + mat2.m[2, 1];
  Result.m[2, 2] := mat1.m[2, 2] + mat2.m[2, 2];
  Result.m[2, 3] := mat1.m[2, 3] + mat2.m[2, 3];
  Result.m[3, 0] := mat1.m[3, 0] + mat2.m[3, 0];
  Result.m[3, 1] := mat1.m[3, 1] + mat2.m[3, 1];
  Result.m[3, 2] := mat1.m[3, 2] + mat2.m[3, 2];
  Result.m[3, 3] := mat1.m[3, 3] + mat2.m[3, 3];
end;

class operator TMat4. +(const mat: TMat4; x: single): TMat4;
begin
  Result.m[0, 0] := mat.m[0, 0] + x;
  Result.m[0, 1] := mat.m[0, 1] + x;
  Result.m[0, 2] := mat.m[0, 2] + x;
  Result.m[0, 3] := mat.m[0, 3] + x;
  Result.m[1, 0] := mat.m[1, 0] + x;
  Result.m[1, 1] := mat.m[1, 1] + x;
  Result.m[1, 2] := mat.m[1, 2] + x;
  Result.m[1, 3] := mat.m[1, 3] + x;
  Result.m[2, 0] := mat.m[2, 0] + x;
  Result.m[2, 1] := mat.m[2, 1] + x;
  Result.m[2, 2] := mat.m[2, 2] + x;
  Result.m[2, 3] := mat.m[2, 3] + x;
  Result.m[3, 0] := mat.m[3, 0] + x;
  Result.m[3, 1] := mat.m[3, 1] + x;
  Result.m[3, 2] := mat.m[3, 2] + x;
  Result.m[3, 3] := mat.m[3, 3] + x;
end;

class operator TMat4. -(const mat1, mat2: TMat4): TMat4;
begin
  Result.m[0, 0] := mat1.m[0, 0] - mat2.m[0, 0];
  Result.m[0, 1] := mat1.m[0, 1] - mat2.m[0, 1];
  Result.m[0, 2] := mat1.m[0, 2] - mat2.m[0, 2];
  Result.m[0, 3] := mat1.m[0, 3] - mat2.m[0, 3];
  Result.m[1, 0] := mat1.m[1, 0] - mat2.m[1, 0];
  Result.m[1, 1] := mat1.m[1, 1] - mat2.m[1, 1];
  Result.m[1, 2] := mat1.m[1, 2] - mat2.m[1, 2];
  Result.m[1, 3] := mat1.m[1, 3] - mat2.m[1, 3];
  Result.m[2, 0] := mat1.m[2, 0] - mat2.m[2, 0];
  Result.m[2, 1] := mat1.m[2, 1] - mat2.m[2, 1];
  Result.m[2, 2] := mat1.m[2, 2] - mat2.m[2, 2];
  Result.m[2, 3] := mat1.m[2, 3] - mat2.m[2, 3];
  Result.m[3, 0] := mat1.m[3, 0] - mat2.m[3, 0];
  Result.m[3, 1] := mat1.m[3, 1] - mat2.m[3, 1];
  Result.m[3, 2] := mat1.m[3, 2] - mat2.m[3, 2];
  Result.m[3, 3] := mat1.m[3, 3] - mat2.m[3, 3];
end;

class operator TMat4. -(const mat: TMat4): TMat4;
begin
  Result.m[0, 0] := -mat.m[0, 0];
  Result.m[0, 1] := -mat.m[0, 1];
  Result.m[0, 2] := -mat.m[0, 2];
  Result.m[0, 3] := -mat.m[0, 3];
  Result.m[1, 0] := -mat.m[1, 0];
  Result.m[1, 1] := -mat.m[1, 1];
  Result.m[1, 2] := -mat.m[1, 2];
  Result.m[1, 3] := -mat.m[1, 3];
  Result.m[2, 0] := -mat.m[2, 0];
  Result.m[2, 1] := -mat.m[2, 1];
  Result.m[2, 2] := -mat.m[2, 2];
  Result.m[2, 3] := -mat.m[2, 3];
  Result.m[3, 0] := -mat.m[3, 0];
  Result.m[3, 1] := -mat.m[3, 1];
  Result.m[3, 2] := -mat.m[3, 2];
  Result.m[3, 3] := -mat.m[3, 3];
end;

class operator TMat4. -(const mat: TMat4; x: single): TMat4;
begin
  Result.m[0, 0] := mat.m[0, 0] - x;
  Result.m[0, 1] := mat.m[0, 1] - x;
  Result.m[0, 2] := mat.m[0, 2] - x;
  Result.m[0, 3] := mat.m[0, 3] - x;
  Result.m[1, 0] := mat.m[1, 0] - x;
  Result.m[1, 1] := mat.m[1, 1] - x;
  Result.m[1, 2] := mat.m[1, 2] - x;
  Result.m[1, 3] := mat.m[1, 3] - x;
  Result.m[2, 0] := mat.m[2, 0] - x;
  Result.m[2, 1] := mat.m[2, 1] - x;
  Result.m[2, 2] := mat.m[2, 2] - x;
  Result.m[2, 3] := mat.m[2, 3] - x;
  Result.m[3, 0] := mat.m[3, 0] - x;
  Result.m[3, 1] := mat.m[3, 1] - x;
  Result.m[3, 2] := mat.m[3, 2] - x;
  Result.m[3, 3] := mat.m[3, 3] - x;
end;

class operator TMat4. / (const mat: TMat4; x: single): TMat4;
begin
  Result.m[0, 0] := mat.m[0, 0] / x;
  Result.m[0, 1] := mat.m[0, 1] / x;
  Result.m[0, 2] := mat.m[0, 2] / x;
  Result.m[0, 3] := mat.m[0, 3] / x;
  Result.m[1, 0] := mat.m[1, 0] / x;
  Result.m[1, 1] := mat.m[1, 1] / x;
  Result.m[1, 2] := mat.m[1, 2] / x;
  Result.m[1, 3] := mat.m[1, 3] / x;
  Result.m[2, 0] := mat.m[2, 0] / x;
  Result.m[2, 1] := mat.m[2, 1] / x;
  Result.m[2, 2] := mat.m[2, 2] / x;
  Result.m[2, 3] := mat.m[2, 3] / x;
  Result.m[3, 0] := mat.m[3, 0] / x;
  Result.m[3, 1] := mat.m[3, 1] / x;
  Result.m[3, 2] := mat.m[3, 2] / x;
  Result.m[3, 3] := mat.m[3, 3] / x;
end;

constructor TMat4.Create(x00, x01, x02, x03, x10, x11, x12, x13, x20, x21, x22,
  x23, x30, x31, x32, x33: single);
begin
  m00 := x00; m01 := x01; m02 := x02; m03 := x03;
  m10 := x10; m11 := x11; m12 := x12; m13 := x13;
  m20 := x20; m21 := x21; m22 := x22; m23 := x23;
  m30 := x30; m31 := x31; m32 := x32; m33 := x33;
end;

function TMat4.GetDeterminant: single;
begin
  Result :=
    (m[0, 0] * m[1, 1] - m[0, 1] * m[1, 0]) * (m[2, 2] * m[3, 3] - m[2, 3] * m[3, 2]) -
    (m[0, 0] * m[1, 2] - m[0, 2] * m[1, 0]) * (m[2, 1] * m[3, 3] - m[2, 3] * m[3, 1]) +
    (m[0, 0] * m[1, 3] - m[0, 3] * m[1, 0]) * (m[2, 1] * m[3, 2] - m[2, 2] * m[3, 1]) +
    (m[0, 1] * m[1, 2] - m[0, 2] * m[1, 1]) * (m[2, 0] * m[3, 3] - m[2, 3] * m[3, 0]) -
    (m[0, 1] * m[1, 3] - m[0, 3] * m[1, 1]) * (m[2, 0] * m[3, 2] - m[2, 2] * m[3, 0]) +
    (m[0, 2] * m[1, 3] - m[0, 3] * m[1, 2]) * (m[2, 0] * m[3, 1] - m[2, 1] * m[3, 0]);
end;

function TMat4.GetColumn(index: byte): TVec4;
var
  res: TVec4;
begin
  case index of
    0: res := TVec4.Create(m00, m01, m02, m03);
    1: res := TVec4.Create(m10, m11, m12, m13);
    2: res := TVec4.Create(m20, m21, m22, m23);
    3: res := TVec4.Create(m30, m31, m32, m33);
    else
      raise Exception.Create(ERROR_4X4);
  end;
  Result := res;
end;

function TMat4.GetRow(index: byte): TVec4;
var
  res: TVec4;
begin
  case index of
    0: res := TVec4.Create(m00, m10, m20, m30);
    1: res := TVec4.Create(m01, m11, m21, m31);
    2: res := TVec4.Create(m02, m11, m22, m32);
    3: res := TVec4.Create(m03, m11, m23, m33);
    else
      raise Exception.Create(ERROR_4X4);
  end;
  Result := res;
end;

procedure TMat4.Init_Identity;
begin
  Self.m := [
    [1, 0, 0, 0],
    [0, 1, 0, 0],
    [0, 0, 1, 0],
    [0, 0, 0, 1]];
end;

procedure TMat4.Init_Zero;
begin
  Self.m := [
    [0, 0, 0, 0],
    [0, 0, 0, 0],
    [0, 0, 0, 0],
    [0, 0, 0, 0]];
end;

function TMat4.inverse(determinant: single): TMat4;
var
  res: TMat4;
begin
  res.Init_Zero;

  determinant := 1 / determinant;

  res.m[0, 0] := determinant * (m[1, 1] * (m[2, 2] * m[3, 3] - m[2, 3] * m[3, 2])
    + m[1, 2] * (m[2, 3] * m[3, 1] - m[2, 1] * m[3, 3])
    + m[1, 3] * (m[2, 1] * m[3, 2] - m[2, 2] * m[3, 1]));

  res.m[0, 1] := determinant * (m[2, 1] * (m[0, 2] * m[3, 3] - m[0, 3] * m[3, 2])
    + m[2, 2] * (m[0, 3] * m[3, 1] - m[0, 1] * m[3, 3])
    + m[2, 3] * (m[0, 1] * m[3, 2] - m[0, 2] * m[3, 1]));

  res.m[0, 2] := determinant * (m[3, 1] * (m[0, 2] * m[1, 3] - m[0, 3] * m[1, 2])
    + m[3, 2] * (m[0, 3] * m[1, 1] - m[0, 1] * m[1, 3])
    + m[3, 3] * (m[0, 1] * m[1, 2] - m[0, 2] * m[1, 1]));

  res.m[0, 3] := determinant * (m[0, 1] * (m[1, 3] * m[2, 2] - m[1, 2] * m[2, 3])
    + m[0, 2] * (m[1, 1] * m[2, 3] - m[1, 3] * m[2, 1])
    + m[0, 3] * (m[1, 2] * m[2, 1] - m[1, 1] * m[2, 2]));

  res.m[1, 0] := determinant * (m[1, 2] * (m[2, 0] * m[3, 3] - m[2, 3] * m[3, 0])
    + m[1, 3] * (m[2, 2] * m[3, 0] - m[2, 0] * m[3, 2])
    + m[1, 0] * (m[2, 3] * m[3, 2] - m[2, 2] * m[3, 3]));

  res.m[1, 1] := determinant * (m[2, 2] * (m[0, 0] * m[3, 3] - m[0, 3] * m[3, 0])
    + m[2, 3] * (m[0, 2] * m[3, 0] - m[0, 0] * m[3, 2])
    + m[2, 0] * (m[0, 3] * m[3, 2] - m[0, 2] * m[3, 3]));

  res.m[1, 2] := determinant * (m[3, 2] * (m[0, 0] * m[1, 3] - m[0, 3] * m[1, 0])
    + m[3, 3] * (m[0, 2] * m[1, 0] - m[0, 0] * m[1, 2])
    + m[3, 0] * (m[0, 3] * m[1, 2] - m[0, 2] * m[1, 3]));

  res.m[1, 3] := determinant * (m[0, 2] * (m[1, 3] * m[2, 0] - m[1, 0] * m[2, 3])
    + m[0, 3] * (m[1, 0] * m[2, 2] - m[1, 2] * m[2, 0])
    + m[0, 0] * (m[1, 2] * m[2, 3] - m[1, 3] * m[2, 2]));

  res.m[2, 0] := determinant * (m[1, 3] * (m[2, 0] * m[3, 1] - m[2, 1] * m[3, 0])
    + m[1, 0] * (m[2, 1] * m[3, 3] - m[2, 3] * m[3, 1])
    + m[1, 1] * (m[2, 3] * m[3, 0] - m[2, 0] * m[3, 3]));

  res.m[2, 1] := determinant * (m[2, 3] * (m[0, 0] * m[3, 1] - m[0, 1] * m[3, 0])
    + m[2, 0] * (m[0, 1] * m[3, 3] - m[0, 3] * m[3, 1])
    + m[2, 1] * (m[0, 3] * m[3, 0] - m[0, 0] * m[3, 3]));

  res.m[2, 2] := determinant * (m[3, 3] * (m[0, 0] * m[1, 1] - m[0, 1] * m[1, 0])
    + m[3, 0] * (m[0, 1] * m[1, 3] - m[0, 3] * m[1, 1])
    + m[3, 1] * (m[0, 3] * m[1, 0] - m[0, 0] * m[1, 3]));

  res.m[2, 3] := determinant * (m[0, 3] * (m[1, 1] * m[2, 0] - m[1, 0] * m[2, 1])
    + m[0, 0] * (m[1, 3] * m[2, 1] - m[1, 1] * m[2, 3])
    + m[0, 1] * (m[1, 0] * m[2, 3] - m[1, 3] * m[2, 0]));

  res.m[3, 0] := determinant * (m[1, 0] * (m[2, 2] * m[3, 1] - m[2, 1] * m[3, 2])
    + m[1, 1] * (m[2, 0] * m[3, 2] - m[2, 2] * m[3, 0])
    + m[1, 2] * (m[2, 1] * m[3, 0] - m[2, 0] * m[3, 1]));

  res.m[3, 1] := determinant * (m[2, 0] * (m[0, 2] * m[3, 1] - m[0, 1] * m[3, 2])
    + m[2, 1] * (m[0, 0] * m[3, 2] - m[0, 2] * m[3, 0])
    + m[2, 2] * (m[0, 1] * m[3, 0] - m[0, 0] * m[3, 1]));

  res.m[3, 2] := determinant * (m[3, 0] * (m[0, 2] * m[1, 1] - m[0, 1] * m[1, 2])
    + m[3, 1] * (m[0, 0] * m[1, 2] - m[0, 2] * m[1, 0])
    + m[3, 2] * (m[0, 1] * m[1, 0] - m[0, 0] * m[1, 1]));

  res.m[3, 3] := determinant * (m[0, 0] * (m[1, 1] * m[2, 2] - m[1, 2] * m[2, 1])
    + m[0, 1] * (m[1, 2] * m[2, 0] - m[1, 0] * m[2, 2])
    + m[0, 2] * (m[1, 0] * m[2, 1] - m[1, 1] * m[2, 0]));

  Result := res;
end;

procedure TMat4.SetColumn(index: byte; const vec: TVec4);
begin
  case index of
    0: begin
      m00 := vec.x; m10 := vec.y; m20 := vec.z; m30 := vec.w;
    end;

    1: begin
      m01 := vec.x; m11 := vec.y; m21 := vec.z; m31 := vec.w;
    end;

    2: begin
      m02 := vec.x; m12 := vec.y; m22 := vec.z; m32 := vec.w;
    end;

    3: begin
      m03 := vec.x; m13 := vec.y; m23 := vec.z; m33 := vec.w;
    end;

    else
      raise Exception.Create(ERROR_4X4); ;
  end;
end;

procedure TMat4.SetRow(index: byte; const vec: TVec4);
begin
  case index of
    0: begin
      m00 := vec.x; m01 := vec.y; m02 := vec.z; m03 := vec.w;
    end;

    1: begin
      m10 := vec.x; m11 := vec.y; m12 := vec.z; m13 := vec.w;
    end;

    2: begin
      m20 := vec.x; m21 := vec.y; m22 := vec.z; m23 := vec.w;
    end;

    3: begin
      m30 := vec.x; m31 := vec.y; m32 := vec.z; m33 := vec.w;
    end;

    else
      raise Exception.Create(ERROR_4X4);
  end;
end;

function TMat4.Transpose: TMat4;
begin
  Result.Create(
    m00, m10, m20, m30,
    m01, m11, m21, m31,
    m02, m12, m22, m32,
    m03, m13, m23, m33);
end;

end.
