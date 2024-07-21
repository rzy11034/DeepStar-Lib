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
  TMat4_Helper = type Helper for TMat4
    // 返回按列优先一维数组的指针
    function ToPtr: PSingle;
  end;

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

    // 返回一个单位矩阵(Identity Matrix)
    class function Mat3_Identity: TMat3;

    // 返回一个空矩阵(Zero Matrix)
    class function Mat3_Zero: TMat3;

    // 使用给定值沿对角线创建一个矩阵
    class function Mat3(x: single): TMat3;

    class function Mat3_Init(x00, x01, x02, x10, x11, x12, x20, x21, x22: single): TMat3;

    // 返回一个单位矩阵(Identity Matrix)
    class function Mat4_Identity: TMat4;  // 返回一个单位矩阵(Identity Matrix)

    // 返回一个空矩阵(Zero Matrix)
    class function Mat4_Zero: TMat4;  // 返回一个空矩阵(Zero Matrix)

    // 使用给定值沿对角线创建一个矩阵
    class function Mat4(x: single): TMat4;

    class function Mat4_Init(x00, x01, x02, x03, x10, x11, x12, x13, x20, x21, x22, x23,
      x30, x31, x32, x33: single): TMat4;

    //═════════════════════════════════════════════════════════════════════════

    // TVec3 向量归一化
    class function Normalize(vec: TVec3): TVec3;

    // 向量点乘：（内积）
    class function Dot(a, b: TVec3): single;

    // 向量叉乘：（外积）
    class function Cross(a, b: TVec3): TVec3;

    //═════════════════════════════════════════════════════════════════════════

    // 角度转弧度值
    class function Radians(deg: single): single;

    // 弧度转角度值
    class function Degrees(Rad: single): single;

    //═════════════════════════════════════════════════════════════════════════
    // TMat4

    // 返回位移矩阵
    class function Translate(m: TMat4; vec: TVec3): TMat4;

    // 返回旋转矩阵
    class function Rotate(m: TMat4; deg: single; vec: TVec3): TMat4;

    // 返回缩放矩阵
    class function Scale(m: TMat4; vec: TVec3): TMat4;

    // 使用视场和创建透视图投影矩阵纵横比，以确定左，右，上，下平面。
    // 这方法类似于现在已弃用的gluPerspective方法。
    // NO（负一对一）：深度值在 -1 和 1 之间归一化。
    class function PerspectiveRH_NO(fovy, aspect, znear, zfar: single): TMat4;
    // 使用视场和创建透视图投影矩阵纵横比，以确定左，右，上，下平面。
    // 这方法类似于现在已弃用的gluPerspective方法。
    // ZO（从零到一）：深度值在 0 和 1 之间归一化。
    class function PerspectiveRH_ZO(fovy, aspect, znear, zfar: single): TMat4;
    
    // 创建一个2D正交投影矩阵。这种方法类似现在已弃用的 gluOrtho2D 方法。
    class function Ortho2D(left, right, bottom, top: single): TMat4;
    // 正交矩阵
    // NO（负一对一）：深度值在 -1 和 1 之间归一化。
    class function OrthoRH_NO(left, right, bottom, top, znear, zfar: single): TMat4;
    // 正交矩阵
    // ZO（从零到一）：深度值在 0 和 1 之间归一化。
    class function OrthoRH_ZO(left, right, bottom, top, znear, zfar: single): TMat4;


    // 平截头体 右手系
    // NO（负一对一）：深度值在 -1 和 1 之间归一化。
    function FrustumRH_NO(left, right, bottom, top, znear, zfar: single): TMat4;
    // 平截头体 右手系
    // ZO（从零到一）：深度值在 0 和 1 之间归一化。
    function FrustumRH_ZO(left, right, bottom, top, znear, zfar: single): TMat4;

    // 视点转换 左手系
    class function LookAtLH(const eyes, center, up: TVec3): TMat4;
    // 视点转换 右手系
    class function LookAtRH(const eyes, center, up: TVec3): TMat4;

    //═════════════════════════════════════════════════════════════════════════

    class function Vec2ToString(VecName: string; v: TVec2): string;
    class function Vec3ToString(VecName: string; v: TVec3): string;
    class function Vec4ToString(VecName: string; v: TVec4): string;
    class function Mat3ToString(matName: string; m: TMat3): string;
    class function Mat4ToString(matName: string; m: TMat4): string;
  end;

implementation

var
  arrSingle16: array[0..15] of single;

{ TMat4_Helper }

function TMat4_Helper.ToPtr: PSingle;
var
  i, j: integer;
  temp: TMat4;
begin
  temp := self;

  for i := 0 to High(temp.Data) do
    for j := 0 to High(temp.Data[0]) do
      arrSingle16[j + i * Length(temp.Data[i])] := temp.Data[i, j];

  Result := @arrSingle16;
end;

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

function TGLM.FrustumRH_NO(left, right, bottom, top, znear, zfar: single): TMat4;
begin
  Result.Init_Zero;
  Result.Data[0, 0] := (2 * znear) / (right - left);
  Result.Data[1, 1] := (2 * znear) / (top - bottom);
  Result.Data[2, 0] := (right + left) / (right - left);
  Result.Data[2, 1] := (top + bottom) / (top - bottom);
  Result.Data[2, 2] := -(zfar + znear) / (zfar - znear);
  Result.Data[2, 3] := -1;
  Result.Data[3, 2] := -2 * zfar * znear / (zfar - znear);
end;

function TGLM.FrustumRH_ZO(left, right, bottom, top, znear, zfar: single): TMat4;
begin
  Result.Init_Zero;
  Result.Data[0, 0] := (2 * znear) / (right - left);
  Result.Data[1, 1] := (2 * znear) / (top - bottom);
  Result.Data[2, 0] := (right + left) / (right - left);
  Result.Data[2, 1] := (top + bottom) / (top - bottom);
  Result.Data[2, 2] := zfar / (znear - zfar);
  Result.Data[2, 3] := -1;
  Result.Data[3, 2] := -(zfar * znear) / (zfar - znear);
end;

class function TGLM.LookAtLH(const eyes, center, up: TVec3): TMat4;
var
  f, s, u: TVec3;
begin
  f := Normalize(center - eyes);
  s := Normalize(Cross(f, up));
  u := Cross(s, f);

  Result := Mat4_Identity;
  Result.Data[0, 0] := s.x;
  Result.Data[1, 0] := s.y;
  Result.Data[2, 0] := s.z;
  Result.Data[0, 1] := u.x;
  Result.Data[1, 1] := u.y;
  Result.Data[2, 1] := u.z;
  Result.Data[0, 2] := f.x;
  Result.Data[1, 2] := f.y;
  Result.Data[2, 2] := f.z;
  Result.Data[3, 0] := -Dot(s, eyes);
  Result.Data[3, 1] := -Dot(u, eyes);
  Result.Data[3, 2] := -Dot(f, eyes);
end;

class function TGLM.LookAtRH(const eyes, center, up: TVec3): TMat4;
var
  f, s, u: TVec3;
begin
  f := Normalize(center - eyes);
  s := Normalize(Cross(f, up));
  u := Cross(s, f);

  Result := Mat4_Identity;
  Result.Data[0, 0] := s.x;
  Result.Data[1, 0] := s.y;
  Result.Data[2, 0] := s.z;
  Result.Data[0, 1] := u.x;
  Result.Data[1, 1] := u.y;
  Result.Data[2, 1] := u.z;
  Result.Data[0, 2] := -f.x;
  Result.Data[1, 2] := -f.y;
  Result.Data[2, 2] := -f.z;
  Result.Data[3, 0] := -Dot(s, eyes);
  Result.Data[3, 1] := -Dot(u, eyes);
  Result.Data[3, 2] := Dot(f, eyes);
end;

class function TGLM.Mat3(x: single): TMat3;
begin
  Result.Create(
    x, 0, 0,
    0, x, 0,
    0, 0, x);
end;

class function TGLM.Mat3ToString(matName: string; m: TMat3): string;
var
  sb: TStringBuilder;
  i, j: integer;
begin
  sb := TStringBuilder.Create();
  try
    sb.Append(matName + ': -------> ' + LineEnding);

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

class function TGLM.Mat4ToString(matName: string; m: TMat4): string;
var
  sb: TStringBuilder;
  i, j: integer;
begin
  sb := TStringBuilder.Create();
  try
    sb.Append(matName + ': -------> ' + LineEnding);

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

class function TGLM.Ortho2D(left, right, bottom, top: single): TMat4;
begin
  Result.Init_Identity;
  Result.Data[0,0] := 2 / (right - left);
  Result.Data[1,1] := 2 / (top - bottom);
  Result.Data[2,2] := -1;
  Result.Data[3,0] := -(right + left) / (right - left);
  Result.Data[3,1] := -(top + bottom) / (top - bottom);
end;

class function TGLM.OrthoRH_NO(left, right, bottom, top, znear, zfar: single): TMat4;
begin
  Result.Init_Identity;
  Result.Data[0, 0] := 2 / (right - left);
  Result.Data[1, 1] := 2 / (top - bottom);
  Result.Data[2, 2] := -2 / (zFar - zNear);
  Result.Data[3, 0] := -(right + left) / (right - left);
  Result.Data[3, 1] := -(top + bottom) / (top - bottom);
  Result.Data[3, 2] := -(zFar + zNear) / (zFar - zNear);
end;

class function TGLM.OrthoRH_ZO(left, right, bottom, top, znear, zfar: single): TMat4;
begin
  Result.Init_Identity;
  Result.Data[0, 0] := 2 / (right - left);
  Result.Data[1, 1] := 2 / (top - bottom);
  Result.Data[2, 2] := -1 / (zFar - zNear);
  Result.Data[3, 0] := -(right + left) / (right - left);
  Result.Data[3, 1] := -(top + bottom) / (top - bottom);
  Result.Data[3, 2] := -zNear / (zFar - zNear);
end;

class function TGLM.PerspectiveRH_NO(fovy, aspect, znear, zfar: single): TMat4;
var
  tanHalfFovy: single;
begin
  tanHalfFovy := Tan(fovy / 2);

  Result.Init_Zero;
  Result.Data[0, 0] := 1 / (aspect * tanHalfFovy);
  Result.Data[1, 1] := 1 / (tanHalfFovy);
  Result.Data[2, 2] := -(zFar + zNear) / (zFar - zNear);
  Result.Data[2, 3] := -1;
  Result.Data[3, 2] := -(2 * zFar * zNear) / (zFar - zNear);
end;

class function TGLM.PerspectiveRH_ZO(fovy, aspect, znear, zfar: single): TMat4;
var
  tanHalfFovy: single;
begin
  tanHalfFovy := tan(fovy / 2);

  Result.Init_Zero;
  Result.Data[0, 0] := 1 / (aspect * tanHalfFovy);
  Result.Data[1, 1] := 1 / (tanHalfFovy);
  Result.Data[2, 2] := zFar / (zFar - zNear);
  Result.Data[2, 3] := 1;
  Result.Data[3, 2] := -(zFar * zNear) / (zFar - zNear);
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
  res.Init_Identity;
  c := Cos(-deg);
  s := Sin(-deg);

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
  //res.init_identity;
  //res.Data[0, 0] := vec.Data[0];
  //res.Data[1, 1] := vec.Data[1];
  //res.Data[2, 2] := vec.Data[2];
  //
  //Result := m * res;


		Result.vecArr[0] := m.vecArr[0] * vec.Data[0];
		Result.vecArr[1] := m.vecArr[1] * vec.Data[1];
		Result.vecArr[2] := m.vecArr[2] * vec.Data[2];
		Result.vecArr[3] := m.vecArr[3];
end;

class function TGLM.Translate(m: TMat4; vec: TVec3): TMat4;
var
  res: TMat4;
begin
  //res.Init_Identity;
  //
  //res.Data[3, 0] += vec.Data[0];
  //res.Data[3, 1] += vec.Data[1];
  //res.Data[3, 2] += vec.Data[2];
  //
  //Result := m * res;

  Result := m;
  Result.vecArr[3] := m.vecArr[0] * vec.Data[0]
    + m.vecArr[1] * vec.Data[1]
    + m.vecArr[2] * vec.Data[2]
    + m.vecArr[3];
end;

class function TGLM.Vec4ToString(VecName: string; v: TVec4): string;
var
  sb: TStringBuilder;
  i: integer;
begin
  sb := TStringBuilder.Create();
  try
    sb.Append(vecName + ': -------> ');

    sb.Append('[');
    for i := 0 to High(v.Data) do
    begin
      sb.Append('%16s', [v.Data[i].ToString]);

      if i <> High(v.Data) then
        sb.Append(', ');
    end;
    sb.AppendLine(']');

    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

class function TGLM.Vec2(x, y: single): TVec2;
begin
  Result.Create(x, y);
end;

class function TGLM.Vec2(x: single): TVec2;
begin
  Result := Vec2(x, x);
end;

class function TGLM.Vec2ToString(VecName: string; v: TVec2): string;
var
  sb: TStringBuilder;
  i: integer;
begin
  sb := TStringBuilder.Create();
  try
    sb.Append(vecName + ': -------> ' + LineEnding);

    sb.Append('[');
    for i := 0 to High(v.Data) do
    begin
      sb.Append('%16s', [v.Data[i].ToString]);

      if i <> High(v.Data) then
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

class function TGLM.Vec3ToString(VecName: string; v: TVec3): string;
var
  sb: TStringBuilder;
  i: integer;
begin
  sb := TStringBuilder.Create();
  try
    sb.Append(vecName + ': -------> ' + LineEnding);

    sb.Append('[');
    for i := 0 to High(v.Data) do
    begin
      sb.Append('%16s', [v.Data[i].ToString]);

      if i <> High(v.Data) then
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
