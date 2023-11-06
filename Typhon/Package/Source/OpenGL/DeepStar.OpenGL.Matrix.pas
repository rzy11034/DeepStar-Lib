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
    class operator +(const m1, m2: TMat3): TMat3;
    class operator +(const m: TMat3; x: single): TMat3;
    class operator -(const m1, m2: TMat3): TMat3;
    class operator -(const m: TMat3): TMat3;
    class operator -(const m: TMat3; x: single): TMat3;
    class operator * (const m1, m2: TMat3): TMat3;
    class operator * (const m: TMat3; v: TVec3): TVec3;
    class operator * (const m: TMat3; x: single): TMat3;
    class operator / (const m: TMat3; x: single): TMat3;

    case integer of
      0: (Data: array[0..2, 0..2] of single);
      1: (m00, m01, m02, m10, m11, m12, m20, m21, m22: single);
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
    class operator +(const m1, m2: TMat4): TMat4;
    class operator +(const m: TMat4; x: single): TMat4;
    class operator -(const m1, m2: TMat4): TMat4;
    class operator -(const m: TMat4): TMat4;
    class operator -(const m: TMat4; x: single): TMat4;
    class operator * (const m1, m2: TMat4): TMat4;
    class operator * (const m: TMat4; v: TVec4): TVec4;
    class operator * (const m: TMat4; x: single): TMat4;
    class operator / (const m: TMat4; x: single): TMat4;

    case integer of
      0: (Data: array[0..3, 0..3] of single);
      1: (m00, m01, m02, m03, m10, m11, m12, m13, m20, m21, m22, m23, m30,
        m31, m32, m33: single);
  end;


implementation

const
  ERROR_3X3 = 'index is not in [0..2]';
  ERROR_4X4 = 'index is not in [0..3]';

  { TMat3 }

class operator TMat3. * (const m1, m2: TMat3): TMat3;
var
  r: array[0..2] of single;
  i: byte;
begin
  for i := 0 to 2 do
  begin
    r := m1.Data[i];
    Result.Data[i, 0] := r[0] * m2.Data[0, 0] + r[1] * m2.Data[1, 0] + r[2] * m2.Data[2, 0];
    Result.Data[i, 1] := r[0] * m2.Data[0, 1] + r[1] * m2.Data[1, 1] + r[2] * m2.Data[2, 1];
    Result.Data[i, 2] := r[0] * m2.Data[0, 2] + r[1] * m2.Data[1, 2] + r[2] * m2.Data[2, 2];
  end;
end;

class operator TMat3. * (const m: TMat3; x: single): TMat3;
begin
  Result.Data[0, 0] := m.Data[0, 0] * x;
  Result.Data[0, 1] := m.Data[0, 1] * x;
  Result.Data[0, 2] := m.Data[0, 2] * x;
  Result.Data[1, 0] := m.Data[1, 0] * x;
  Result.Data[1, 1] := m.Data[1, 1] * x;
  Result.Data[1, 2] := m.Data[1, 2] * x;
  Result.Data[2, 0] := m.Data[2, 0] * x;
  Result.Data[2, 1] := m.Data[2, 1] * x;
  Result.Data[2, 2] := m.Data[2, 2] * x;
end;

class operator TMat3. * (const m: TMat3; v: TVec3): TVec3;
begin
  Result.Data[0] := m.Data[0, 0] * v.Data[0] + m.Data[0, 1] * v.Data[1] + m.Data[0, 2] * v.Data[2];
  Result.Data[1] := m.Data[1, 0] * v.Data[0] + m.Data[1, 1] * v.Data[1] + m.Data[1, 2] * v.Data[2];
  Result.Data[2] := m.Data[2, 0] * v.Data[0] + m.Data[2, 1] * v.Data[1] + m.Data[2, 2] * v.Data[2];
end;

class operator TMat3. +(const m1, m2: TMat3): TMat3;
begin
  Result.Data[0, 0] := m1.Data[0, 0] + m2.Data[0, 0];
  Result.Data[0, 1] := m1.Data[0, 1] + m2.Data[0, 1];
  Result.Data[0, 2] := m1.Data[0, 2] + m2.Data[0, 2];
  Result.Data[1, 0] := m1.Data[1, 0] + m2.Data[1, 0];
  Result.Data[1, 1] := m1.Data[1, 1] + m2.Data[1, 1];
  Result.Data[1, 2] := m1.Data[1, 2] + m2.Data[1, 2];
  Result.Data[2, 0] := m1.Data[2, 0] + m2.Data[2, 0];
  Result.Data[2, 1] := m1.Data[2, 1] + m2.Data[2, 1];
  Result.Data[2, 2] := m1.Data[2, 2] + m2.Data[2, 2];
end;

class operator TMat3. +(const m: TMat3; x: single): TMat3;
begin
  Result.Data[0, 0] := m.Data[0, 0] + x;
  Result.Data[0, 1] := m.Data[0, 1] + x;
  Result.Data[0, 2] := m.Data[0, 2] + x;
  Result.Data[1, 0] := m.Data[1, 0] + x;
  Result.Data[1, 1] := m.Data[1, 1] + x;
  Result.Data[1, 2] := m.Data[1, 2] + x;
  Result.Data[2, 0] := m.Data[2, 0] + x;
  Result.Data[2, 1] := m.Data[2, 1] + x;
  Result.Data[2, 2] := m.Data[2, 2] + x;
end;

class operator TMat3. -(const m1, m2: TMat3): TMat3;
begin
  Result.Data[0, 0] := m1.Data[0, 0] - m2.Data[0, 0];
  Result.Data[0, 1] := m1.Data[0, 1] - m2.Data[0, 1];
  Result.Data[0, 2] := m1.Data[0, 2] - m2.Data[0, 2];
  Result.Data[1, 0] := m1.Data[1, 0] - m2.Data[1, 0];
  Result.Data[1, 1] := m1.Data[1, 1] - m2.Data[1, 1];
  Result.Data[1, 2] := m1.Data[1, 2] - m2.Data[1, 2];
  Result.Data[2, 0] := m1.Data[2, 0] - m2.Data[2, 0];
  Result.Data[2, 1] := m1.Data[2, 1] - m2.Data[2, 1];
  Result.Data[2, 2] := m1.Data[2, 2] - m2.Data[2, 2];
end;

class operator TMat3. -(const m: TMat3): TMat3;
begin
  Result.Data[0, 0] := -m.Data[0, 0];
  Result.Data[0, 1] := -m.Data[0, 1];
  Result.Data[0, 2] := -m.Data[0, 2];
  Result.Data[1, 0] := -m.Data[1, 0];
  Result.Data[1, 1] := -m.Data[1, 1];
  Result.Data[1, 2] := -m.Data[1, 2];
  Result.Data[2, 0] := -m.Data[2, 0];
  Result.Data[2, 1] := -m.Data[2, 1];
  Result.Data[2, 2] := -m.Data[2, 2];
end;

class operator TMat3. -(const m: TMat3; x: single): TMat3;
begin
  Result.Data[0, 0] := m.Data[0, 0] - x;
  Result.Data[0, 1] := m.Data[0, 1] - x;
  Result.Data[0, 2] := m.Data[0, 2] - x;
  Result.Data[1, 0] := m.Data[1, 0] - x;
  Result.Data[1, 1] := m.Data[1, 1] - x;
  Result.Data[1, 2] := m.Data[1, 2] - x;
  Result.Data[2, 0] := m.Data[2, 0] - x;
  Result.Data[2, 1] := m.Data[2, 1] - x;
  Result.Data[2, 2] := m.Data[2, 2] - x;
end;

class operator TMat3. / (const m: TMat3; x: single): TMat3;
begin
  Result.Data[0, 0] := m.Data[0, 0] / x;
  Result.Data[0, 1] := m.Data[0, 1] / x;
  Result.Data[0, 2] := m.Data[0, 2] / x;
  Result.Data[1, 0] := m.Data[1, 0] / x;
  Result.Data[1, 1] := m.Data[1, 1] / x;
  Result.Data[1, 2] := m.Data[1, 2] / x;
  Result.Data[2, 0] := m.Data[2, 0] / x;
  Result.Data[2, 1] := m.Data[2, 1] / x;
  Result.Data[2, 2] := m.Data[2, 2] / x;
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
    Data[0, 0] * (Data[1, 1] * Data[2, 2] - Data[1, 2] * Data[2, 1]) -
    Data[0, 1] * (Data[1, 0] * Data[2, 2] - Data[1, 2] * Data[2, 0]) +
    Data[0, 2] * (Data[1, 0] * Data[2, 1] - Data[1, 1] * Data[2, 0]);
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
  Self.Data := [
    [1, 0, 0],
    [0, 1, 0],
    [0, 0, 1]];
end;

procedure TMat3.Init_Zero;
begin
  Self.Data := [
    [0, 0, 0],
    [0, 0, 0],
    [0, 0, 0]];
end;

function TMat3.inverse(determinant: single): TMat3;
begin
  determinant := 1 / determinant;
  Result.Data[0, 0] := (Data[1, 1] * Data[2, 2] - Data[2, 1] * Data[1, 2]) * determinant;
  Result.Data[0, 1] := -(Data[0, 1] * Data[2, 2] - Data[2, 1] * Data[0, 2]) * determinant;
  Result.Data[0, 2] := (Data[0, 1] * Data[1, 2] - Data[1, 1] * Data[0, 2]) * determinant;
  Result.Data[1, 0] := -(Data[1, 0] * Data[2, 2] - Data[2, 0] * Data[1, 2]) * determinant;
  Result.Data[1, 1] := (Data[0, 0] * Data[2, 2] - Data[2, 0] * Data[0, 2]) * determinant;
  Result.Data[1, 2] := -(Data[0, 0] * Data[1, 2] - Data[1, 0] * Data[0, 2]) * determinant;
  Result.Data[2, 0] := (Data[1, 0] * Data[2, 1] - Data[2, 0] * Data[1, 1]) * determinant;
  Result.Data[2, 1] := -(Data[0, 0] * Data[2, 1] - Data[2, 0] * Data[0, 1]) * determinant;
  Result.Data[2, 2] := (Data[0, 0] * Data[1, 1] - Data[1, 0] * Data[0, 1]) * determinant;
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
  Result := Create(
    m00, m10, m20,
    m01, m11, m21,
    m02, m12, m22);
end;


{ TMat4 }

class operator TMat4. * (const m1, m2: TMat4): TMat4;
var
  r: array[0..3] of single;
  i: byte;
begin
  for i := 0 to 3 do
  begin
    r := m1.Data[i];

    Result.Data[i, 0] :=
      r[0] * m2.Data[0, 0] + r[1] * m2.Data[1, 0] + r[2] * m2.Data[2, 0] + r[3] * m2.Data[3, 0];

    Result.Data[i, 1] :=
      r[0] * m2.Data[0, 1] + r[1] * m2.Data[1, 1] + r[2] * m2.Data[2, 1] + r[3] * m2.Data[3, 1];

    Result.Data[i, 2] :=
      r[0] * m2.Data[0, 2] + r[1] * m2.Data[1, 2] + r[2] * m2.Data[2, 2] + r[3] * m2.Data[3, 2];

    Result.Data[i, 3] :=
      r[0] * m2.Data[0, 3] + r[1] * m2.Data[1, 3] + r[2] * m2.Data[2, 3] + r[3] * m2.Data[3, 3];
  end;
end;

class operator TMat4. * (const m: TMat4; x: single): TMat4;
begin
  Result.Data[0, 0] := m.Data[0, 0] * x;
  Result.Data[0, 1] := m.Data[0, 1] * x;
  Result.Data[0, 2] := m.Data[0, 2] * x;
  Result.Data[0, 3] := m.Data[0, 3] * x;
  Result.Data[1, 0] := m.Data[1, 0] * x;
  Result.Data[1, 1] := m.Data[1, 1] * x;
  Result.Data[1, 2] := m.Data[1, 2] * x;
  Result.Data[1, 3] := m.Data[1, 3] * x;
  Result.Data[2, 0] := m.Data[2, 0] * x;
  Result.Data[2, 1] := m.Data[2, 1] * x;
  Result.Data[2, 2] := m.Data[2, 2] * x;
  Result.Data[2, 3] := m.Data[2, 3] * x;
  Result.Data[3, 0] := m.Data[3, 0] * x;
  Result.Data[3, 1] := m.Data[3, 1] * x;
  Result.Data[3, 2] := m.Data[3, 2] * x;
  Result.Data[3, 3] := m.Data[3, 3] * x;
end;

class operator TMat4. * (const m: TMat4; v: TVec4): TVec4;
begin
  Result.Data[0] := m.Data[0, 0] * v.Data[0] + m.Data[0, 1] * v.Data[1]
    + m.Data[0, 2] * v.Data[2] + m.Data[0, 3] * v.Data[3];

  Result.Data[1] := m.Data[1, 0] * v.Data[0] + m.Data[1, 1] * v.Data[1]
    + m.Data[1, 2] * v.Data[2] + m.Data[1, 3] * v.Data[3];

  Result.Data[2] := m.Data[2, 0] * v.Data[0] + m.Data[2, 1] * v.Data[1]
    + m.Data[2, 2] * v.Data[2] + m.Data[2, 3] * v.Data[3];

  Result.Data[3] := m.Data[3, 0] * v.Data[0] + m.Data[3, 1] * v.Data[1]
    + m.Data[3, 2] * v.Data[2] + m.Data[3, 3] * v.Data[3];
end;

class operator TMat4. +(const m1, m2: TMat4): TMat4;
begin
  Result.Data[0, 0] := m1.Data[0, 0] + m2.Data[0, 0];
  Result.Data[0, 1] := m1.Data[0, 1] + m2.Data[0, 1];
  Result.Data[0, 2] := m1.Data[0, 2] + m2.Data[0, 2];
  Result.Data[0, 3] := m1.Data[0, 3] + m2.Data[0, 3];
  Result.Data[1, 0] := m1.Data[1, 0] + m2.Data[1, 0];
  Result.Data[1, 1] := m1.Data[1, 1] + m2.Data[1, 1];
  Result.Data[1, 2] := m1.Data[1, 2] + m2.Data[1, 2];
  Result.Data[1, 3] := m1.Data[1, 3] + m2.Data[1, 3];
  Result.Data[2, 0] := m1.Data[2, 0] + m2.Data[2, 0];
  Result.Data[2, 1] := m1.Data[2, 1] + m2.Data[2, 1];
  Result.Data[2, 2] := m1.Data[2, 2] + m2.Data[2, 2];
  Result.Data[2, 3] := m1.Data[2, 3] + m2.Data[2, 3];
  Result.Data[3, 0] := m1.Data[3, 0] + m2.Data[3, 0];
  Result.Data[3, 1] := m1.Data[3, 1] + m2.Data[3, 1];
  Result.Data[3, 2] := m1.Data[3, 2] + m2.Data[3, 2];
  Result.Data[3, 3] := m1.Data[3, 3] + m2.Data[3, 3];
end;

class operator TMat4. +(const m: TMat4; x: single): TMat4;
begin
  Result.Data[0, 0] := m.Data[0, 0] + x;
  Result.Data[0, 1] := m.Data[0, 1] + x;
  Result.Data[0, 2] := m.Data[0, 2] + x;
  Result.Data[0, 3] := m.Data[0, 3] + x;
  Result.Data[1, 0] := m.Data[1, 0] + x;
  Result.Data[1, 1] := m.Data[1, 1] + x;
  Result.Data[1, 2] := m.Data[1, 2] + x;
  Result.Data[1, 3] := m.Data[1, 3] + x;
  Result.Data[2, 0] := m.Data[2, 0] + x;
  Result.Data[2, 1] := m.Data[2, 1] + x;
  Result.Data[2, 2] := m.Data[2, 2] + x;
  Result.Data[2, 3] := m.Data[2, 3] + x;
  Result.Data[3, 0] := m.Data[3, 0] + x;
  Result.Data[3, 1] := m.Data[3, 1] + x;
  Result.Data[3, 2] := m.Data[3, 2] + x;
  Result.Data[3, 3] := m.Data[3, 3] + x;
end;

class operator TMat4. -(const m1, m2: TMat4): TMat4;
begin
  Result.Data[0, 0] := m1.Data[0, 0] - m2.Data[0, 0];
  Result.Data[0, 1] := m1.Data[0, 1] - m2.Data[0, 1];
  Result.Data[0, 2] := m1.Data[0, 2] - m2.Data[0, 2];
  Result.Data[0, 3] := m1.Data[0, 3] - m2.Data[0, 3];
  Result.Data[1, 0] := m1.Data[1, 0] - m2.Data[1, 0];
  Result.Data[1, 1] := m1.Data[1, 1] - m2.Data[1, 1];
  Result.Data[1, 2] := m1.Data[1, 2] - m2.Data[1, 2];
  Result.Data[1, 3] := m1.Data[1, 3] - m2.Data[1, 3];
  Result.Data[2, 0] := m1.Data[2, 0] - m2.Data[2, 0];
  Result.Data[2, 1] := m1.Data[2, 1] - m2.Data[2, 1];
  Result.Data[2, 2] := m1.Data[2, 2] - m2.Data[2, 2];
  Result.Data[2, 3] := m1.Data[2, 3] - m2.Data[2, 3];
  Result.Data[3, 0] := m1.Data[3, 0] - m2.Data[3, 0];
  Result.Data[3, 1] := m1.Data[3, 1] - m2.Data[3, 1];
  Result.Data[3, 2] := m1.Data[3, 2] - m2.Data[3, 2];
  Result.Data[3, 3] := m1.Data[3, 3] - m2.Data[3, 3];
end;

class operator TMat4. -(const m: TMat4): TMat4;
begin
  Result.Data[0, 0] := -m.Data[0, 0];
  Result.Data[0, 1] := -m.Data[0, 1];
  Result.Data[0, 2] := -m.Data[0, 2];
  Result.Data[0, 3] := -m.Data[0, 3];
  Result.Data[1, 0] := -m.Data[1, 0];
  Result.Data[1, 1] := -m.Data[1, 1];
  Result.Data[1, 2] := -m.Data[1, 2];
  Result.Data[1, 3] := -m.Data[1, 3];
  Result.Data[2, 0] := -m.Data[2, 0];
  Result.Data[2, 1] := -m.Data[2, 1];
  Result.Data[2, 2] := -m.Data[2, 2];
  Result.Data[2, 3] := -m.Data[2, 3];
  Result.Data[3, 0] := -m.Data[3, 0];
  Result.Data[3, 1] := -m.Data[3, 1];
  Result.Data[3, 2] := -m.Data[3, 2];
  Result.Data[3, 3] := -m.Data[3, 3];
end;

class operator TMat4. -(const m: TMat4; x: single): TMat4;
begin
  Result.Data[0, 0] := m.Data[0, 0] - x;
  Result.Data[0, 1] := m.Data[0, 1] - x;
  Result.Data[0, 2] := m.Data[0, 2] - x;
  Result.Data[0, 3] := m.Data[0, 3] - x;
  Result.Data[1, 0] := m.Data[1, 0] - x;
  Result.Data[1, 1] := m.Data[1, 1] - x;
  Result.Data[1, 2] := m.Data[1, 2] - x;
  Result.Data[1, 3] := m.Data[1, 3] - x;
  Result.Data[2, 0] := m.Data[2, 0] - x;
  Result.Data[2, 1] := m.Data[2, 1] - x;
  Result.Data[2, 2] := m.Data[2, 2] - x;
  Result.Data[2, 3] := m.Data[2, 3] - x;
  Result.Data[3, 0] := m.Data[3, 0] - x;
  Result.Data[3, 1] := m.Data[3, 1] - x;
  Result.Data[3, 2] := m.Data[3, 2] - x;
  Result.Data[3, 3] := m.Data[3, 3] - x;
end;

class operator TMat4. / (const m: TMat4; x: single): TMat4;
begin
  Result.Data[0, 0] := m.Data[0, 0] / x;
  Result.Data[0, 1] := m.Data[0, 1] / x;
  Result.Data[0, 2] := m.Data[0, 2] / x;
  Result.Data[0, 3] := m.Data[0, 3] / x;
  Result.Data[1, 0] := m.Data[1, 0] / x;
  Result.Data[1, 1] := m.Data[1, 1] / x;
  Result.Data[1, 2] := m.Data[1, 2] / x;
  Result.Data[1, 3] := m.Data[1, 3] / x;
  Result.Data[2, 0] := m.Data[2, 0] / x;
  Result.Data[2, 1] := m.Data[2, 1] / x;
  Result.Data[2, 2] := m.Data[2, 2] / x;
  Result.Data[2, 3] := m.Data[2, 3] / x;
  Result.Data[3, 0] := m.Data[3, 0] / x;
  Result.Data[3, 1] := m.Data[3, 1] / x;
  Result.Data[3, 2] := m.Data[3, 2] / x;
  Result.Data[3, 3] := m.Data[3, 3] / x;
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
    (Data[0, 0] * Data[1, 1] - Data[0, 1] * Data[1, 0]) * (Data[2, 2] * Data[3, 3] - Data[2, 3] * Data[3, 2]) -
    (Data[0, 0] * Data[1, 2] - Data[0, 2] * Data[1, 0]) * (Data[2, 1] * Data[3, 3] - Data[2, 3] * Data[3, 1]) +
    (Data[0, 0] * Data[1, 3] - Data[0, 3] * Data[1, 0]) * (Data[2, 1] * Data[3, 2] - Data[2, 2] * Data[3, 1]) +
    (Data[0, 1] * Data[1, 2] - Data[0, 2] * Data[1, 1]) * (Data[2, 0] * Data[3, 3] - Data[2, 3] * Data[3, 0]) -
    (Data[0, 1] * Data[1, 3] - Data[0, 3] * Data[1, 1]) * (Data[2, 0] * Data[3, 2] - Data[2, 2] * Data[3, 0]) +
    (Data[0, 2] * Data[1, 3] - Data[0, 3] * Data[1, 2]) * (Data[2, 0] * Data[3, 1] - Data[2, 1] * Data[3, 0]);
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
  Self.Data := [
    [1, 0, 0, 0],
    [0, 1, 0, 0],
    [0, 0, 1, 0],
    [0, 0, 0, 1]];
end;

procedure TMat4.Init_Zero;
begin
  Self.Data := [
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

  res.Data[0, 0] := determinant * (
    Data[1, 1] * (Data[2, 2] * Data[3, 3] - Data[2, 3] * Data[3, 2]) +
    Data[1, 2] * (Data[2, 3] * Data[3, 1] - Data[2, 1] * Data[3, 3]) +
    Data[1, 3] * (Data[2, 1] * Data[3, 2] - Data[2, 2] * Data[3, 1]));

  res.Data[0, 1] := determinant * (Data[2, 1] * (
    Data[0, 2] * Data[3, 3] - Data[0, 3] * Data[3, 2]) +
    Data[2, 2] * (Data[0, 3] * Data[3, 1] - Data[0, 1] * Data[3, 3]) +
    Data[2, 3] * (Data[0, 1] * Data[3, 2] - Data[0, 2] * Data[3, 1]));

  res.Data[0, 2] := determinant * (Data[3, 1] * (
    Data[0, 2] * Data[1, 3] - Data[0, 3] * Data[1, 2]) +
    Data[3, 2] * (Data[0, 3] * Data[1, 1] - Data[0, 1] * Data[1, 3]) +
    Data[3, 3] * (Data[0, 1] * Data[1, 2] - Data[0, 2] * Data[1, 1]));

  res.Data[0, 3] := determinant * (Data[0, 1] * (
    Data[1, 3] * Data[2, 2] - Data[1, 2] * Data[2, 3]) +
    Data[0, 2] * (Data[1, 1] * Data[2, 3] - Data[1, 3] * Data[2, 1]) +
    Data[0, 3] * (Data[1, 2] * Data[2, 1] - Data[1, 1] * Data[2, 2]));

  res.Data[1, 0] := determinant * (Data[1, 2] * (
    Data[2, 0] * Data[3, 3] - Data[2, 3] * Data[3, 0]) +
    Data[1, 3] * (Data[2, 2] * Data[3, 0] - Data[2, 0] * Data[3, 2]) +
    Data[1, 0] * (Data[2, 3] * Data[3, 2] - Data[2, 2] * Data[3, 3]));

  res.Data[1, 1] := determinant * (Data[2, 2] * (
    Data[0, 0] * Data[3, 3] - Data[0, 3] * Data[3, 0]) +
    Data[2, 3] * (Data[0, 2] * Data[3, 0] - Data[0, 0] * Data[3, 2]) +
    Data[2, 0] * (Data[0, 3] * Data[3, 2] - Data[0, 2] * Data[3, 3]));

  res.Data[1, 2] := determinant * (Data[3, 2] * (
    Data[0, 0] * Data[1, 3] - Data[0, 3] * Data[1, 0]) +
    Data[3, 3] * (Data[0, 2] * Data[1, 0] - Data[0, 0] * Data[1, 2]) +
    Data[3, 0] * (Data[0, 3] * Data[1, 2] - Data[0, 2] * Data[1, 3]));

  res.Data[1, 3] := determinant * (Data[0, 2] * (
    Data[1, 3] * Data[2, 0] - Data[1, 0] * Data[2, 3]) +
    Data[0, 3] * (Data[1, 0] * Data[2, 2] - Data[1, 2] * Data[2, 0]) +
    Data[0, 0] * (Data[1, 2] * Data[2, 3] - Data[1, 3] * Data[2, 2]));

  res.Data[2, 0] := determinant * (Data[1, 3] * (
    Data[2, 0] * Data[3, 1] - Data[2, 1] * Data[3, 0]) +
    Data[1, 0] * (Data[2, 1] * Data[3, 3] - Data[2, 3] * Data[3, 1]) +
    Data[1, 1] * (Data[2, 3] * Data[3, 0] - Data[2, 0] * Data[3, 3]));

  res.Data[2, 1] := determinant * (Data[2, 3] * (
    Data[0, 0] * Data[3, 1] - Data[0, 1] * Data[3, 0]) +
    Data[2, 0] * (Data[0, 1] * Data[3, 3] - Data[0, 3] * Data[3, 1]) +
    Data[2, 1] * (Data[0, 3] * Data[3, 0] - Data[0, 0] * Data[3, 3]));

  res.Data[2, 2] := determinant * (Data[3, 3] * (
    Data[0, 0] * Data[1, 1] - Data[0, 1] * Data[1, 0]) +
    Data[3, 0] * (Data[0, 1] * Data[1, 3] - Data[0, 3] * Data[1, 1]) +
    Data[3, 1] * (Data[0, 3] * Data[1, 0] - Data[0, 0] * Data[1, 3]));

  res.Data[2, 3] := determinant * (Data[0, 3] * (
    Data[1, 1] * Data[2, 0] - Data[1, 0] * Data[2, 1]) +
    Data[0, 0] * (Data[1, 3] * Data[2, 1] - Data[1, 1] * Data[2, 3]) +
    Data[0, 1] * (Data[1, 0] * Data[2, 3] - Data[1, 3] * Data[2, 0]));

  res.Data[3, 0] := determinant * (Data[1, 0] * (
    Data[2, 2] * Data[3, 1] - Data[2, 1] * Data[3, 2]) +
    Data[1, 1] * (Data[2, 0] * Data[3, 2] - Data[2, 2] * Data[3, 0]) +
    Data[1, 2] * (Data[2, 1] * Data[3, 0] - Data[2, 0] * Data[3, 1]));

  res.Data[3, 1] := determinant * (
    Data[2, 0] * (Data[0, 2] * Data[3, 1] - Data[0, 1] * Data[3, 2]) +
    Data[2, 1] * (Data[0, 0] * Data[3, 2] - Data[0, 2] * Data[3, 0]) +
    Data[2, 2] * (Data[0, 1] * Data[3, 0] - Data[0, 0] * Data[3, 1]));

  res.Data[3, 2] := determinant * (Data[3, 0] * (
    Data[0, 2] * Data[1, 1] - Data[0, 1] * Data[1, 2]) +
    Data[3, 1] * (Data[0, 0] * Data[1, 2] - Data[0, 2] * Data[1, 0]) +
    Data[3, 2] * (Data[0, 1] * Data[1, 0] - Data[0, 0] * Data[1, 1]));

  res.Data[3, 3] := determinant * (Data[0, 0] * (
    Data[1, 1] * Data[2, 2] - Data[1, 2] * Data[2, 1]) +
    Data[0, 1] * (Data[1, 2] * Data[2, 0] - Data[1, 0] * Data[2, 2]) +
    Data[0, 2] * (Data[1, 0] * Data[2, 1] - Data[1, 1] * Data[2, 0]));

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
      raise Exception.Create(ERROR_4X4); ;
  end;
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
