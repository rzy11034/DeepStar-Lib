unit DeepStar.OpenGL.GLM;

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
  private type
    TArr_Vec3f = array[0..2] of single;

  public
    // 返回一个单位矩阵(Identity Matrix)
    class function Mat4_Identity: TMat4; static;
    // 返回一个空矩阵(Zero Matrix)
    class function Mat4_Zero: TMat4; static;
    // 返回位移矩阵
    class function Translate(m: TMat4; vec3: TArr_Vec3f): TMat4; static;
    // 返回X轴旋转矩阵
    class function RotateX(m: TMat4; degree: single): TMat4; static;
    // 返回X轴旋转矩阵
    class function RotateY(m: TMat4; degree: single): TMat4; static;
    // 返回X轴旋转矩阵
    class function RotateZ(m: TMat4; degree: single): TMat4; static;
    // 返回缩放矩阵
    class function Scale(m: TMat4; vec3: TArr_Vec3f): TMat4; static;
    // 按列优先返回一个一维数组指针
    class function ValuePtr(const m: TMat4): TArr_Single16; static;
  end;

implementation

{ TGLM }

class function TGLM.Mat4_Identity: TMat4;
begin
  Result.init_identity;
end;

class function TGLM.Mat4_Zero: TMat4;
begin
  Result.init_zero;
end;

class function TGLM.RotateX(m: TMat4; degree: single): TMat4;
var
  theta: Float;
  res: TMat4;
begin
  theta := DegToRad(degree);
  res := TGLM.Mat4_Identity;

  res.Data := [
    [1, 0, 0, 0],
    [Cos(theta), -Sin(theta), 0, 0],
    [Sin(theta), Cos(theta), 0, 0],
    [0, 0, 0, 1]];

  Result := m * res;
end;

class function TGLM.RotateY(m: TMat4; degree: single): TMat4;
var
  theta: Float;
  res: TMat4;
begin
  theta := DegToRad(degree);
  res := TGLM.Mat4_Identity;

  res.Data := [
    [Cos(theta), Sin(theta), 0, 0],
    [0, 1, 0, 0],
    [-Sin(theta), Cos(theta), 0, 0],
    [0, 0, 0, 1]];

  Result := m * res;
end;

class function TGLM.RotateZ(m: TMat4; degree: single): TMat4;
var
  theta: Float;
  res: TMat4;
begin
  theta := DegToRad(degree);
  res := TGLM.Mat4_Identity;

  res.Data := [
    [Cos(theta), Sin(theta), 0, 0],
    [-Sin(theta), Cos(theta), 0, 0],
    [0, 0, 1, 0],
    [0, 0, 0, 1]];

  Result := m * res;
end;

class function TGLM.Scale(m: TMat4; vec3: TArr_Vec3f): TMat4;
var
  temp: TMat4;
begin
  temp.init_identity;
  temp.Data[0, 0] := vec3[0];
  temp.Data[1, 1] := vec3[1];
  temp.Data[2, 2] := vec3[2];

  Result := m * temp;
end;

class function TGLM.Translate(m: TMat4; vec3: TArr_Vec3f): TMat4;
var
  v: TVec4;
  res: TMat4;
begin
  res.init_identity;

  res.Data[0, 3] := vec3[0];
  res.Data[1, 3] := vec3[1];
  res.Data[2, 3] := vec3[2];
  res := m * res;

  Result := res;
end;

class function TGLM.ValuePtr(const m: TMat4): TArr_Single16;
var
  res: array[0..15] of single;
  i, j: integer;
  temp: TMat4;
begin
  temp := m.transpose;

  for i := 0 to High(temp.Data) do
    for j := 0 to High(temp.Data[0]) do
      res[j + i * Length(temp.Data[i])] := temp.Data[i, j];

  Result := res;
end;

end.
