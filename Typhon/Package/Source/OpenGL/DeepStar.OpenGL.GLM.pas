unit DeepStar.OpenGL.GLM;

{$mode ObjFPC}{$H+}
{$ModeSwitch unicodestrings}{$J-}

interface

uses
  Classes,
  SysUtils,
  Math,
  DeepStar.UString,
  DeepStar.OpenGL.Vector,
  DeepStar.OpenGL.Matrix;

type
  // 向量
  TVec2 = DeepStar.OpenGL.Vector.TVec2;
  TVec3 = DeepStar.OpenGL.Vector.TVec3;
  TVec4 = DeepStar.OpenGL.Vector.TVec4;
  // 矩阵
  TMat3 = DeepStar.OpenGL.Matrix.TMat3;
  TMat4 = DeepStar.OpenGL.Matrix.TMat4;

  TArr_Single16 = array[0..15] of single;

  // 线性代数辅助类
  TGLM = class(TObject)
  public
    class function Vec2(x, y: single): TVec2;
    class function Vec3(x, y, z: single): TVec3;
    class function Vec4(x, y, z, w: single): TVec4;

    class function Mat3_Identity: TMat3;  // 返回一个单位矩阵(Identity Matrix)
    class function Mat3_Zero: TMat3;  // 返回一个空矩阵(Zero Matrix)
    class function Mat3_Init(x00, x01, x02, x10, x11, x12, x20, x21, x22: single): TMat3;

    class function Mat4_Identity: TMat4;  // 返回一个单位矩阵(Identity Matrix)
    class function Mat4_Zero: TMat4;  // 返回一个空矩阵(Zero Matrix)
    class function Mat4_Init(x00, x01, x02, x03, x10, x11, x12, x13, x20, x21, x22, x23,
      x30, x31, x32, x33: single): TMat4;

    //═════════════════════════════════════════════════════════════════════════

    // 向量归一化
    class function Normalize(vec: TVec3): TVec3;
    // 向量点乘：（内积）
    class function Dot(a, b: TVec3): single;
    // 向量叉乘：（外积）
    class function Cross(a, b: TVec3): TVec3;

    //═════════════════════════════════════════════════════════════════════════

    // 返回弧度值
    class function Radians(deg: single): single;
    // 返回角度值
    class function Degrees(Rad: single): single;

    //═════════════════════════════════════════════════════════════════════════
    // TMat4

    // 返回位移矩阵
    class function Translate(m: TMat4; vec: TVec3): TMat4;
    // 返回旋转矩阵
    class function Rotate(m: TMat4; deg: single; vec: TVec3): TMat4;
    // 返回缩放矩阵
    class function Scale(m: TMat4; vec: TVec3): TMat4;
    // 按列优先返回一个一维数组指针
    class function ValuePtr(const m: TMat4): TArr_Single16;
    // 使用视场和创建透视图投影矩阵纵横比，以确定左，右，上，下平面。这
    // 方法类似于现在已弃用的gluPerspective方法。
    class function Perspective(fovy, aspect, znear, zfar: single): TMat4;
    // 正交矩阵
    class function Ortho(left, right, bottom, top, znear, zfar: single): TMat4;
    // 平截头体
    class function Frustum(left, right, bottom, top, znear, zfar: single): TMat4;
    // 视点转换
    class function LookAt(eyes, center, up: TVec3): TMat4;
    // 创建一个2D正交投影矩阵。这种方法与现在类似已弃用的gluOrtho2D方法。
    class function Ortho2d(left, right, bottom, top: single): TMat4;

    //═════════════════════════════════════════════════════════════════════════

    class function Mat4ToString(matName: string; m: TMat4): string;
  end;

implementation

{ TGLM }

class function TGLM.Cross(a, b: TVec3): TVec3;
begin
  Result := a >< b;
end;

class function TGLM.Degrees(Rad: single): single;
begin
  Result := RadToDeg(Rad);
end;

class function TGLM.Dot(a, b: TVec3): single;
begin
  Result := a ** b;
end;

class function TGLM.Frustum(left, right, bottom, top, znear, zfar: single): TMat4;
begin
  Result := Mat4_Zero;
  Result.Data[0, 0] := 2 * znear / (right - left);
  Result.Data[1, 1] := 2 * znear / (top - bottom);
  Result.Data[2, 0] := (right + left) / (right - left);
  Result.Data[2, 1] := (top + bottom) / (top - bottom);
  Result.Data[2, 2] := -(zfar + znear) / (zfar - znear);
  Result.Data[2, 3] := -1.0;
  Result.Data[3, 2] := -2 * zfar * znear / (zfar - znear);
  Result.Data[3, 3] := 0.0;
end;

class function TGLM.LookAt(eyes, center, up: TVec3): TMat4;
var
  f, s, u: TVec3;
begin
  f := Normalize(center - eyes);
  s := Normalize(Cross(f, Normalize(up)));
  u := Cross(s, f);

  Result := Mat4_Init(
    s.Data[0], s.Data[1], s.Data[2], -Dot(s, eyes),
    u.Data[0], u.Data[1], u.Data[2], -Dot(u, eyes),
    -f.Data[0], -f.Data[1], -f.Data[2], Dot(f, eyes),
    0, 0, 0, 1);
end;

class function TGLM.Mat3_Identity: TMat3;
begin
  Result.Init_Identity;
end;

class function TGLM.Mat3_Init(x00, x01, x02, x10, x11, x12, x20, x21, x22: single): TMat3;
begin
  Result := TMat3.Create(x00, x01, x02, x10, x11, x12, x20, x21, x22);
end;

class function TGLM.Mat3_Zero: TMat3;
begin
  Result.Init_Zero;
end;

class function TGLM.Mat4ToString(matName: string; m: TMat4): string;
var
  sb: TStringBuilder;
  i, j: integer;
begin
  sb := TStringBuilder.Create();
  try
    sb.AppendLine(matName + ': -------> ');

    for i := 0 to High(m.Data) do
    begin
      sb.Append('[');
      for j := 0 to High(m.Data[i]) do
      begin
        sb.Append('%16s', [m.Data[i, j].ToString]);

        if j <> High(m.Data[i]) then
          sb.Append(', ');
      end;

      if i <> High(m.Data) then
        sb.AppendLine('], ')
      else
        sb.AppendLine(']');
    end;

    sb.AppendLine;
    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

class function TGLM.Mat4_Identity: TMat4;
begin
  Result.init_identity;
end;

class function TGLM.Mat4_Init(x00, x01, x02, x03, x10, x11, x12, x13, x20, x21, x22, x23,
  x30, x31, x32, x33: single): TMat4;
begin
  Result := TMat4.Create(x00, x01, x02, x03, x10, x11, x12, x13, x20, x21, x22, x23,
    x30, x31, x32, x33);
end;

class function TGLM.Mat4_Zero: TMat4;
begin
  Result.init_zero;
end;

class function TGLM.Normalize(vec: TVec3): TVec3;
var
  len, oneOverLen: single;
  res: TVec3;
begin
  len := vec.length;

  if len <= 0 then
    res.Create(1, 0, 0);

  oneOverLen := 1 / len;
  res.Create(vec.Data[0] * oneOverLen, vec.Data[1] * oneOverLen, vec.Data[2] * oneOverLen);

  Result := res;
end;

class function TGLM.Ortho(left, right, bottom, top, znear, zfar: single): TMat4;
var
  m00, m11, m22, m30, m31, m32: single;
  res: TMat4;
begin
  m00 := single(2 / (right - left));
  m11 := single(2 / (top - bottom));
  m22 := single(-2 / (zFar - zNear));
  m30 := single(-(right + left) / (right - left));
  m31 := single(-(top + bottom) / (top - bottom));
  m32 := single(-(zFar + zNear) / (zFar - zNear));

  Result := Mat4_Init(
    m00, 0, 0, m30,
    0, m11, 0, m31,
    0, 0, m22, m32,
    0, 0, 0, 1);
end;

class function TGLM.Ortho2d(left, right, bottom, top: single): TMat4;
var
  m00, m11, m22, m30, m31: single;
begin
  m00 := single(2 / (right - left));
  m11 := single(2 / (top - bottom));
  m22 := single(-1);
  m30 := single(-(right + left) / (right - left));
  m31 := single(-(top + bottom) / (top - bottom));

  Result := Mat4_Init(
    m00, 0, 0, m30,
    0, m11, 0, m31,
    0, 0, m22, 0,
    0, 0, 0, 1);
end;

class function TGLM.Perspective(fovy, aspect, znear, zfar: single): TMat4;
var
  right, top: single;
begin
  top := znear * Tan(fovy / 2);
  right := top * aspect;
  Result := Frustum(-right, right, -top, top, znear, zfar).transpose;
end;

class function TGLM.Radians(deg: single): single;
begin
  Result := DegToRad(deg);
end;

class function TGLM.Rotate(m: TMat4; deg: single; vec: TVec3): TMat4;
var
  c, s, x, y, z: single;
  res: Tmat4;
begin
  //Mat4_Init rotationMatrix(vec3 axis, float angle)
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
  c := Cos(deg);
  s := Sin(deg);

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
  Result.Create(x, y);
end;

class function TGLM.Vec3(x, y, z: single): TVec3;
begin
  Result.Create(x, y, z);
end;

class function TGLM.Vec4(x, y, z, w: single): TVec4;
begin
  Result.Create(x, y, z, w);
end;

end.
