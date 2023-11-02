﻿unit DeepStar.OpenGL.GLM;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Math,
  matrix;

type
  // 向量
  TVec2 = Tvector2_single;
  TVec3 = Tvector3_single;
  TVec4 = Tvector4_single;
  // 矩阵
  TMat2 = Tmatrix2_single;
  TMat3 = Tmatrix3_single;
  TMat4 = Tmatrix4_single;

  TArr_Single16 = array[0..15] of single;

  // 线性代数辅助类
  TGLM = class(TObject)
  public
    class function Vec2(x, y: single): TVec2; static;
    class function Vec3(x, y, z: single): TVec3; static;
    class function Vec4(x, y, z, w: single): TVec4; static;
    class function Mat4_Identity: TMat4; static; // 返回一个单位矩阵(Identity Matrix)
    class function Mat4_Zero: TMat4; static; // 返回一个空矩阵(Zero Matrix)
    class function Mat4_Init(aa, ab, ac, ad, ba, bb, bc, bd, ca, cb, cc, cd, da,
      db, dc, dd: single): TMat4; static;

    class function Normalize(vec: TVec3): TVec3;

    // 返回弧度值
    class function Radians(degrees: single): single; static;

    // 返回位移矩阵
    class function Translate(m: TMat4; vec: TVec3): TMat4; static;
    // 返回旋转矩阵
    class function Rotate(m: TMat4; degrees: single; vec: TVec3): TMat4; static;
    // 返回缩放矩阵
    class function Scale(m: TMat4; vec: TVec3): TMat4; static;
    // 按列优先返回一个一维数组指针
    class function ValuePtr(const m: TMat4): TArr_Single16; static;
    // 透视矩阵
    class function Perspective(fovy, aspect, znear, zfar: single): TMat4; static;
    // 正交矩阵
    class function Ortho(left, right, bottom, top, znear, zfar: single): TMat4; static;
    // 平截头体
    class function Frustum(left, right, bottom, top, znear, zfar: single): TMat4; static;
  end;

implementation

{ TGLM }

class function TGLM.Frustum(left, right, bottom, top, znear, zfar: single): TMat4;
begin
  Result := Mat4_Identity;
  Result.Data[0, 0] := 2 * znear / (right - left);
  Result.Data[1, 1] := 2 * znear / (top - bottom);
  Result.Data[2, 0] := (right + left) / (right - left);
  Result.Data[2, 1] := (top + bottom) / (top - bottom);
  Result.Data[2, 2] := -(zfar + znear) / (zfar - znear);
  Result.Data[2, 3] := -1.0;
  Result.Data[3, 2] := -2 * zfar * znear / (zfar - znear);
  Result.Data[3, 3] := 0.0;
end;

class function TGLM.Mat4_Identity: TMat4;
begin
  Result.init_identity;
end;

class function TGLM.Mat4_Init(aa, ab, ac, ad, ba, bb, bc, bd, ca, cb, cc, cd, da,
  db, dc, dd: single): TMat4;
begin
  Result.init(aa, ab, ac, ad, ba, bb, bc, bd, ca, cb, cc, cd, da, db, dc, dd);
end;

class function TGLM.Mat4_Zero: TMat4;
begin
  Result.init_zero;
end;

class function TGLM.Normalize(vec: TVec3): TVec3;

  //function __fun1__: TVec3;
  //var
  //  l: single;
  //begin
  //  l := Sqrt((vec.Data[0]) + (vec.Data[1]) + (vec.Data[2]));
  //  if l = 0.0 then l := 1.0;
  //  vec /= l;
  //
  //  Result := vec;
  //end;

  function __fun2__: TVec3;
  var
    len, oneOverLen: single;
    res: TVec3;
  begin
    len := vec.length;

    if len <= 0 then
      res.init(1, 0, 0);

    oneOverLen := 1 / len;
    res.init(vec.Data[0] * oneOverLen, vec.Data[1] * oneOverLen, vec.Data[2] * oneOverLen);
    Result := res;
  end;

begin
  Result := __fun2__;
end;

class function TGLM.Ortho(left, right, bottom, top, znear, zfar: single): TMat4;
var
  res:TMat4;
begin
  res := Mat4_Identity;
  res.Data[0, 0] := 2 / (right - left);
  res.Data[1, 1] := 2 / (top - bottom);
  res.Data[2, 2] := -2 / (zfar - znear);
  res.Data[3, 0] := -(right + left) / (right - left);
  res.Data[3, 1] := -(top + bottom) / (top - bottom);
  res.Data[3, 2] := -(zfar + znear) / (zfar - znear);

  Result := res;
end;

class function TGLM.Perspective(fovy, aspect, znear, zfar: single): TMat4;
var
  p, right, top: single;
begin
  p := fovy * Pi / 360;
  top := znear * Tan(p);
  right := top * aspect;
  Result := Frustum(-right, right, -top, top, znear, zfar);
end;

class function TGLM.Radians(degrees: single): single;
begin
  Result := DegToRad(degrees);
end;

class function TGLM.Rotate(m: TMat4; degrees: single; vec: TVec3): TMat4;
var
  c, s, x, y, z: single;
  res: Tmat4;
begin
  //mat4 rotationMatrix(vec3 axis, float angle)
  //{
  //    axis = normalize(axis);
  //    float s = sin(angle);
  //    float c = cos(angle);
  //    float oc = 1.0 - c;
  //
  //    return mat4(oc * axis.x * axis.x + c,           oc * axis.x * axis.y - axis.z * s,  oc * axis.z * axis.x + axis.y * s,  0.0,
  //                oc * axis.x * axis.y + axis.z * s,  oc * axis.y * axis.y + c,           oc * axis.y * axis.z - axis.x * s,  0.0,
  //                oc * axis.z * axis.x - axis.y * s,  oc * axis.y * axis.z + axis.x * s,  oc * axis.z * axis.z + c,           0.0,
  //                0.0,                                0.0,                                0.0,                                1.0);

  res := TGLM.Mat4_Identity;
  c := Cos(degrees);
  s := Sin(degrees);

  vec := Normalize(vec);
  x := vec.Data[0];
  y := vec.Data[1];
  z := vec.Data[2];

  res.Data[0, 0] := x * x * (1 - c) + c;
  res.Data[1, 0] := x * y * (1 - c) + z * s;
  res.Data[2, 0] := x * z * (1 - c) - y * s;

  res.Data[0, 1] := x * y * (1 - c) - z * s;
  res.Data[1, 1] := y * y * (1 - c) + c;
  res.Data[2, 1] := y * z * (1 - c) + x * s;

  res.Data[0, 2] := z * x * (1 - c) + y * s;
  res.Data[1, 2] := y * z * (1 - c) - x * s;
  res.Data[2, 2] := z * z * (1 - c) + c;

  Result := m * res;
end;

class function TGLM.Scale(m: TMat4; vec: TVec3): TMat4;
var
  res: TMat4;
begin
  res.init_identity;
  res.Data[0, 0] := vec.Data[0];
  res.Data[1, 1] := vec.Data[1];
  res.Data[2, 2] := vec.Data[2];

  Result := m * res;
end;

class function TGLM.Translate(m: TMat4; vec: TVec3): TMat4;
var
  res: TMat4;
begin
  res.init_identity;

  res.Data[0, 3] += vec.Data[0];
  res.Data[1, 3] += vec.Data[1];
  res.Data[2, 3] += vec.Data[2];

  Result := m * res;
end;

class function TGLM.ValuePtr(const m: TMat4): TArr_Single16;
var
  res: TArr_Single16;
  i, j: integer;
  temp: TMat4;
begin
  temp := m.transpose;

  for i := 0 to High(temp.Data) do
    for j := 0 to High(temp.Data[0]) do
      res[j + i * Length(temp.Data[i])] := temp.Data[i, j];

  Result := res;
end;

class function TGLM.Vec2(x, y: single): TVec2;
begin
  Result.init(x, y);
end;

class function TGLM.Vec3(x, y, z: single): TVec3;
begin
  Result.init(x, y, z);
end;

class function TGLM.Vec4(x, y, z, w: single): TVec4;
begin
  Result.init(x, y, z, w);
end;

end.
