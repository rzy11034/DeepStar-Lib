unit DeepStar.OpenGL.GLM;

{$mode ObjFPC}{$H+}
{$ModeSwitch unicodestrings}{$J-}
{$ModeSwitch advancedrecords}{$J-}
{$modeswitch typehelpers}

interface

uses
  Classes,
  SysUtils,
  Math,
  DeepStar.Utils,
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

type
  // 线性代数辅助类
  TGLM = class(TObject)
  public
    class function Vec2(x: single): TVec2;
    class function Vec2(x, y: single): TVec2;

    class function Vec3(x: single): TVec3;
    class function Vec3(x, y, z: single): TVec3;

    class function Vec4(x: single): TVec4;
    class function Vec4(x, y, z, w: single): TVec4;

    //═════════════════════════════════════════════════════════════════════════

    // 返回一个3X3单位矩阵(Identity Matrix)
    class function Mat3_Identity: TMat3;

    // 返回一个3X3空矩阵(Zero Matrix)
    class function Mat3_Zero: TMat3;

    // 使用给定值沿对角线创建一个 3X3 矩阵
    class function Mat3(x: single): TMat3;

    class function Mat3_Init(x00, x01, x02, x10, x11, x12, x20, x21, x22: single): TMat3;

    // 返回一个4X4单位矩阵(Identity Matrix)
    class function Mat4_Identity: TMat4;  // 返回一个单位矩阵(Identity Matrix)

    // 返回一个4X4空矩阵(Zero Matrix)
    class function Mat4_Zero: TMat4;  // 返回一个空矩阵(Zero Matrix)

    // 使用给定值沿对角线创建一个4X4矩阵
    class function Mat4(x: single): TMat4;

    class function Mat4_Init(x00, x01, x02, x03, x10, x11, x12, x13, x20, x21, x22, x23,
      x30, x31, x32, x33: single): TMat4;

    //═════════════════════════════════════════════════════════════════════════
    // Math

    // 角度转弧度值
    class function Radians(deg: single): single;

    // 弧度转角度值
    class function Degrees(Rad: single): single;

    //═════════════════════════════════════════════════════════════════════════
    // TVec2

    //═════════════════════════════════════════════════════════════════════════
    // TVec3

    // TVec3 向量归一化
    class function Normalize(vec: TVec3): TVec3;

    // 向量点乘：（内积）
    class function Dot(a, b: TVec3): single;

    // 向量叉乘：（外积）
    class function Cross(a, b: TVec3): TVec3;

    // 绕X轴旋转一个三维向量。
    class function RotateX(vec: TVec3; angle: single): TVec3;
    // 绕Y轴旋转一个三维向量。
    class function RotateY(vec: TVec3; angle: single): TVec3;
    // 绕Z轴旋转一个三维向量。
    class function RotateZ(vec: TVec3; angle: single): TVec3;

    //═════════════════════════════════════════════════════════════════════════
    // TMat3

     //class function Rotate(mat: TMat3; angle: single): TMat3;

    //═════════════════════════════════════════════════════════════════════════
    // TMat4

    // 返回位移矩阵
    class function Translate(mat: TMat4; vec: TVec3): TMat4;

    // 返回旋转矩阵
    class function Rotate(mat: TMat4; angle: single; vec: TVec3): TMat4;

    // 返回缩放矩阵
    class function Scale(mat: TMat4; vec: TVec3): TMat4;

    // 使用视场和创建透视图投影矩阵纵横比，以确定左，右，上，下平面。
    // 这方法类似于现在已弃用的gluPerspective方法。
    class function Perspective(fovy, aspect, znear, zfar: single): TMat4;

    // 创建一个2D正交投影矩阵。这种方法类似现在已弃用的 gluOrtho2D 方法。
    class function Ortho2D(left, right, bottom, top: single): TMat4;

    // 正交矩阵
    class function Ortho(left, right, bottom, top, znear, zfar: single): TMat4;

    // 平截头体
    class function Frustum(left, right, bottom, top, znear, zfar: single): TMat4;

    // 视点转换
    class function LookAt(const eyes, center, up: TVec3): TMat4;

    //═════════════════════════════════════════════════════════════════════════

    // 返回 mat3x3 的指针
    class function ValuePtr(const mat: TMat3): PSingle;
    // 返回 mat4x4 的指针
    class function ValuePtr(const mat: TMat4): PSingle;

    //═════════════════════════════════════════════════════════════════════════

    class function ToString(vecName: string; vec: TVec2): string;
    class function ToString(vecName: string; vec: TVec3): string;
    class function ToString(vecName: string; vec: TVec4): string;
    class function ToString(matName: string; mat: TMat3): string;
    class function ToString(matName: string; mat: TMat4): string;

    function ToString: String; reintroduce;
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
  // NO（负一对一）：深度值在 -1 和 1 之间归一化。
  function __FrustumRH_NO__: TMat4;
  begin
    Result.Init_Zero;
    Result.m[0, 0] := (2 * znear) / (right - left);
    Result.m[1, 1] := (2 * znear) / (top - bottom);
    Result.m[2, 0] := (right + left) / (right - left);
    Result.m[2, 1] := (top + bottom) / (top - bottom);
    Result.m[2, 2] := -(zfar + znear) / (zfar - znear);
    Result.m[2, 3] := -1;
    Result.m[3, 2] := -2 * zfar * znear / (zfar - znear);
  end;

  // ZO（从零到一）：深度值在 0 和 1 之间归一化。
  function __FrustumRH_ZO__: TMat4;
  begin
    Result.Init_Zero;
    Result.m[0, 0] := (2 * znear) / (right - left);
    Result.m[1, 1] := (2 * znear) / (top - bottom);
    Result.m[2, 0] := (right + left) / (right - left);
    Result.m[2, 1] := (top + bottom) / (top - bottom);
    Result.m[2, 2] := zfar / (znear - zfar);
    Result.m[2, 3] := -1;
    Result.m[3, 2] := -(zfar * znear) / (zfar - znear);
  end;

begin
  Result := __FrustumRH_NO__;
end;

class function TGLM.LookAt(const eyes, center, up: TVec3): TMat4;
  // 左手系
  function __LookAtLH__: TMat4;
  var
    f, s, u: TVec3;
  begin
    f := Normalize(center - eyes);
    s := Normalize(Cross(f, up));
    u := Cross(s, f);

    Result := Mat4_Identity;
    Result.m[0, 0] := s.x;
    Result.m[1, 0] := s.y;
    Result.m[2, 0] := s.z;
    Result.m[0, 1] := u.x;
    Result.m[1, 1] := u.y;
    Result.m[2, 1] := u.z;
    Result.m[0, 2] := f.x;
    Result.m[1, 2] := f.y;
    Result.m[2, 2] := f.z;
    Result.m[3, 0] := -Dot(s, eyes);
    Result.m[3, 1] := -Dot(u, eyes);
    Result.m[3, 2] := -Dot(f, eyes);
  end;

  // 右手系
  function __LookAtRH__: TMat4;
  var
    f, s, u: TVec3;
  begin
    f := Normalize(center - eyes);
    s := Normalize(Cross(f, up));
    u := Cross(s, f);

    Result := Mat4_Identity;
    Result.m[0, 0] := s.x;
    Result.m[1, 0] := s.y;
    Result.m[2, 0] := s.z;
    Result.m[0, 1] := u.x;
    Result.m[1, 1] := u.y;
    Result.m[2, 1] := u.z;
    Result.m[0, 2] := -f.x;
    Result.m[1, 2] := -f.y;
    Result.m[2, 2] := -f.z;
    Result.m[3, 0] := -Dot(s, eyes);
    Result.m[3, 1] := -Dot(u, eyes);
    Result.m[3, 2] := Dot(f, eyes);
  end;

begin
  Result := __LookAtRH__;
end;

class function TGLM.Mat3(x: single): TMat3;
begin
  Result.Create(
    x, 0, 0,
    0, x, 0,
    0, 0, x);
end;

class function TGLM.ToString(matName: string; mat: TMat3): string;
var
  sb: TStringBuilder;
  i, j: integer;
begin
  sb := TStringBuilder.Create();
  try
    sb.Append(matName + ': -------> ' + LineEnding);

    for i := 0 to High(mat.m) do
    begin
      sb.Append('[');
      for j := 0 to High(mat.m[i]) do
      begin
        sb.Append('%16s', [mat.m[i, j].ToString]);

        if j <> High(mat.m[i]) then
          sb.Append(', ');
      end;

      if i <> High(mat.m) then
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

class function TGLM.Mat3_Identity: TMat3;
begin
  Result.Init_Identity;
end;

class function TGLM.Mat3_Init(x00, x01, x02, x10, x11, x12, x20, x21, x22: single): TMat3;
begin
  Result.Create(x00, x01, x02, x10, x11, x12, x20, x21, x22);
end;

class function TGLM.Mat3_Zero: TMat3;
begin
  Result.Init_Zero;
end;

class function TGLM.Mat4(x: single): TMat4;
begin
  Result.Create(
    x, 0, 0, 0,
    0, x, 0, 0,
    0, 0, x, 0,
    0, 0, 0, x);
end;

class function TGLM.ToString(matName: string; mat: TMat4): string;
var
  sb: TStringBuilder;
  i, j: integer;
begin
  sb := TStringBuilder.Create();
  try
    sb.Append(matName + ': -------> ' + LineEnding);

    for i := 0 to High(mat.m) do
    begin
      sb.Append('[');
      for j := 0 to High(mat.m[i]) do
      begin
        sb.Append('%16s', [mat.m[i, j].ToString]);

        if j <> High(mat.m[i]) then
          sb.Append(', ');
      end;

      if i <> High(mat.m) then
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
  Result.Init_Identity;
end;

class function TGLM.Mat4_Init(x00, x01, x02, x03, x10, x11, x12, x13, x20, x21, x22, x23,
  x30, x31, x32, x33: single): TMat4;
begin
  Result.Create(x00, x01, x02, x03, x10, x11, x12, x13, x20, x21, x22, x23,
    x30, x31, x32, x33);
end;

class function TGLM.Mat4_Zero: TMat4;
begin
  Result.Init_Zero;
end;

class function TGLM.Normalize(vec: TVec3): TVec3;
var
  len, oneOverLen: single;
  res: TVec3;
begin
  len := vec.length;
  res := vec;

  if len <= 0 then
    len := 1;

  oneOverLen := 1 / len;
  res *= oneOverLen;

  Result := res;
end;

class function TGLM.Ortho(left, right, bottom, top, znear, zfar: single): TMat4;
  // NO（负一对一）：深度值在 -1 和 1 之间归一化。
  function __OrthoRH_NO__: TMat4;
  begin
    Result.Init_Identity;
    Result.m[0, 0] := 2 / (right - left);
    Result.m[1, 1] := 2 / (top - bottom);
    Result.m[2, 2] := -2 / (zFar - zNear);
    Result.m[3, 0] := -(right + left) / (right - left);
    Result.m[3, 1] := -(top + bottom) / (top - bottom);
    Result.m[3, 2] := -(zFar + zNear) / (zFar - zNear);
  end;

  // ZO（从零到一）：深度值在 0 和 1 之间归一化。
  function __OrthoRH_ZO__: TMat4;
  begin
    Result.Init_Identity;
    Result.m[0, 0] := 2 / (right - left);
    Result.m[1, 1] := 2 / (top - bottom);
    Result.m[2, 2] := -1 / (zFar - zNear);
    Result.m[3, 0] := -(right + left) / (right - left);
    Result.m[3, 1] := -(top + bottom) / (top - bottom);
    Result.m[3, 2] := -zNear / (zFar - zNear);
  end;

begin
  Result := __OrthoRH_NO__;
end;

class function TGLM.Ortho2D(left, right, bottom, top: single): TMat4;
begin
  Result.Init_Identity;
  Result.m[0, 0] := 2 / (right - left);
  Result.m[1, 1] := 2 / (top - bottom);
  Result.m[2, 2] := -1;
  Result.m[3, 0] := -(right + left) / (right - left);
  Result.m[3, 1] := -(top + bottom) / (top - bottom);
end;

class function TGLM.Perspective(fovy, aspect, znear, zfar: single): TMat4;
  // NO（负一对一）：深度值在 -1 和 1 之间归一化。
  function __PerspectiveRH_NO__: TMat4;
  var
    tanHalfFovy: single;
  begin
    tanHalfFovy := Tan(fovy / 2);

    Result.Init_Zero;
    Result.m[0, 0] := 1 / (aspect * tanHalfFovy);
    Result.m[1, 1] := 1 / (tanHalfFovy);
    Result.m[2, 2] := -(zFar + zNear) / (zFar - zNear);
    Result.m[2, 3] := -1;
    Result.m[3, 2] := -(2 * zFar * zNear) / (zFar - zNear);
  end;

  // ZO（从零到一）：深度值在 0 和 1 之间归一化。
  function __PerspectiveRH_ZO__: TMat4;
  var
    tanHalfFovy: single;
  begin
    tanHalfFovy := tan(fovy / 2);

    Result.Init_Zero;
    Result.m[0, 0] := 1 / (aspect * tanHalfFovy);
    Result.m[1, 1] := 1 / (tanHalfFovy);
    Result.m[2, 2] := zFar / (zFar - zNear);
    Result.m[2, 3] := 1;
    Result.m[3, 2] := -(zFar * zNear) / (zFar - zNear);
  end;

begin
  Result := __PerspectiveRH_NO__;
end;

class function TGLM.Radians(deg: single): single;
begin
  Result := DegToRad(deg);
end;

class function TGLM.RotateX(vec: TVec3; angle: single): TVec3;
var
  sin_, cos_: single;
begin
  Result := vec;

  sin_ := single(Sin(angle));
  cos_ := single(Cos(angle));

  Result.y := vec.y * cos_ - vec.z * sin_;
  Result.z := vec.y * sin_ + vec.z * cos_;
end;

class function TGLM.RotateY(vec: TVec3; angle: single): TVec3;
var
  cos_, sin_: single;
begin
  Result := vec;

  cos_ := single(Cos(angle));
  sin_ := single(Sin(angle));

  Result.x := vec.x * cos_ + vec.z * sin_;
  Result.z := -vec.x * sin_ + vec.z * cos_;
end;

class function TGLM.RotateZ(vec: TVec3; angle: single): TVec3;
var
  cos_, sin_: single;
begin
  Result := vec;

  cos_ := single(Cos(angle));
  sin_ := single(Sin(angle));

  Result.x := vec.x * cos_ - vec.y * sin_;
  Result.y := vec.x * sin_ + vec.y * cos_;
end;

class function TGLM.Rotate(mat: TMat4; angle: single; vec: TVec3): TMat4;
  function __Rotate__: TMat4;
  var
    a, c, s: single;
    axis, temp: TVec3;
    matR: TMat4;
  begin
    a := angle;
    c := Cos(a);
    s := Sin(a);

    axis := Normalize(vec);
    temp := TVec3((1 - c) * axis);

    matR := Mat4_Zero;
    matR.m[0,0] := c + temp.v[0] * axis.v[0];
    matR.m[0,1] := temp.v[0] * axis.v[1] + s * axis.v[2];
    matR.m[0,2] := temp.v[0] * axis.v[2] - s * axis.v[1];

    matR.m[1,0] := temp.v[1] * axis.v[0] - s * axis.v[2];
    matR.m[1,1] := c + temp.v[1] * axis.v[1];
    matR.m[1,2] := temp.v[1] * axis.v[2] + s * axis.v[0];

    matR.m[2,0] := temp.v[2] * axis.v[0] + s * axis.v[1];
    matR.m[2,1] := temp.v[2] * axis.v[1] - s * axis.v[0];
    matR.m[2,2] := c + temp.v[2] * axis.v[2];

    Result.Init_Zero;
    Result.v[0] := mat.v[0] * matR.m[0][0] + mat.v[1] * matR.m[0][1] + mat.v[2] * matR.m[0][2];
    Result.v[1] := mat.v[0] * matR.m[1][0] + mat.v[1] * matR.m[1][1] + mat.v[2] * matR.m[1][2];
    Result.v[2] := mat.v[0] * matR.m[2][0] + mat.v[1] * matR.m[2][1] + mat.v[2] * matR.m[2][2];
    Result.v[3] := mat.v[3];
  end;

  function __Rotate_slow__: TMat4;
  var
    a, c, s: single;
    axis: TVec3;
  begin
    a := angle;
    c := Cos(a);
    s := Sin(a);

    axis := normalize(vec);

    Result.m[0, 0] := c + (1 - c) * axis.x * axis.x;
    Result.m[0, 1] := (1 - c) * axis.x * axis.y + s * axis.z;
    Result.m[0, 2] := (1 - c) * axis.x * axis.z - s * axis.y;
    Result.m[0, 3] := 0;

    Result.m[1, 0] := (1 - c) * axis.y * axis.x - s * axis.z;
    Result.m[1, 1] := c + (1 - c) * axis.y * axis.y;
    Result.m[1, 2] := (1 - c) * axis.y * axis.z + s * axis.x;
    Result.m[1, 3] := 0;

    Result.m[2, 0] := (1 - c) * axis.z * axis.x + s * axis.y;
    Result.m[2, 1] := (1 - c) * axis.z * axis.y - s * axis.x;
    Result.m[2, 2] := c + (1 - c) * axis.z * axis.z;
    Result.m[2, 3] := 0;

    Result.v[3] := Vec4(0, 0, 0, 1);

    Result := mat * Result;
  end;

begin
  Result := __Rotate__;
end;

class function TGLM.Scale(mat: TMat4; vec: TVec3): TMat4;
begin
  Result.v[0] := mat.v[0] * vec.v[0];
  Result.v[1] := mat.v[1] * vec.v[1];
  Result.v[2] := mat.v[2] * vec.v[2];
  Result.v[3] := mat.v[3];
end;

class function TGLM.Translate(mat: TMat4; vec: TVec3): TMat4;
begin
  Result := mat;
  Result.v[3] := mat.v[0] * vec.v[0]
    + mat.v[1] * vec.v[1]
    + mat.v[2] * vec.v[2]
    + mat.v[3];
end;

class function TGLM.ValuePtr(const mat: TMat3): PSingle;
begin
  Result := @mat.m;
end;

class function TGLM.ValuePtr(const mat: TMat4): PSingle;
begin
  Result := @mat.m;
end;

class function TGLM.ToString(vecName: string; vec: TVec4): string;
var
  sb: TStringBuilder;
  i: integer;
begin
  sb := TStringBuilder.Create();
  try
    sb.Append(vecName + ': -------> ');

    sb.Append('[');
    for i := 0 to High(vec.v) do
    begin
      sb.Append('%16s', [vec.v[i].ToString]);

      if i <> High(vec.v) then
        sb.Append(', ');
    end;
    sb.AppendLine(']');

    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

function TGLM.ToString: String;
var
  res: AnsiString;
begin
  res := inherited ToString;
  Result := string(res);
end;

class function TGLM.Vec2(x, y: single): TVec2;
begin
  Result.Create(x, y);
end;

class function TGLM.Vec2(x: single): TVec2;
begin
  Result := Vec2(x, x);
end;

class function TGLM.ToString(vecName: string; vec: TVec2): string;
var
  sb: TStringBuilder;
  i: integer;
begin
  sb := TStringBuilder.Create();
  try
    sb.Append(vecName + ': -------> ' + LineEnding);

    sb.Append('[');
    for i := 0 to High(vec.v) do
    begin
      sb.Append('%16s', [vec.v[i].ToString]);

      if i <> High(vec.v) then
        sb.Append(', ');
    end;
    sb.AppendLine(']');

    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

class function TGLM.Vec3(x, y, z: single): TVec3;
begin
  Result.Create(x, y, z);
end;

class function TGLM.Vec3(x: single): TVec3;
begin
  Result := Vec3(x, x, x);
end;

class function TGLM.ToString(vecName: string; vec: TVec3): string;
var
  sb: TStringBuilder;
  i: integer;
begin
  sb := TStringBuilder.Create();
  try
    sb.Append(vecName + ': -------> ' + LineEnding);

    sb.Append('[');
    for i := 0 to High(vec.v) do
    begin
      sb.Append('%16s', [vec.v[i].ToString]);

      if i <> High(vec.v) then
        sb.Append(', ');
    end;
    sb.AppendLine(']');

    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

class function TGLM.Vec4(x, y, z, w: single): TVec4;
begin
  Result.Create(x, y, z, w);
end;

class function TGLM.Vec4(x: single): TVec4;
begin
  Result := Vec4(x, x, x, x);
end;

end.
