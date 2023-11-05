unit DeepStar.OpenGL.Vector;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}{$J-}

interface

uses
  Classes,
  SysUtils,
  DeepStar.OpenGL.GLAD_GL;

type
  TVec2 = packed record
    constructor Create(v0, v1: GLfloat);
    procedure InitZero;
    procedure InitOne;
    function Length: single;
    function SquaredLength: single;
    class operator +(const v: TVec2; const scalar: GLfloat): TVec2;
    class operator +(const scalar: GLfloat; const v: TVec2): TVec2;
    class operator +(const av, bv: TVec2): TVec2;
    class operator -(const v: TVec2): TVec2;
    class operator -(const v: TVec2; const scalar: GLfloat): TVec2;
    class operator -(const av, bv: TVec2): TVec2;
    class operator * (const v: TVec2; const scalar: GLfloat): TVec2;
    class operator * (const scalar: GLfloat; const v: TVec2): TVec2;
    class operator * (const av, bv: TVec2): TVec2;
    class operator / (const v: TVec2; const scalar: GLfloat): TVec2;
    class operator / (const av, bv: TVec2): TVec2;
    // 向量点乘：（内积）
    class operator ** (const av, bv: TVec2): GLfloat;

    case integer of
      0: (x, y: GLfloat);
      1: (Data: array[0..1] of GLfloat);
  end;

  TVec3 = packed record
    constructor Create(v0, v1, v2: GLfloat);
    procedure InitZero;
    procedure InitOne;
    function Length: single;
    function SquaredLength: single;
    class operator +(const v: TVec3; const scalar: GLfloat): TVec3;
    class operator +(const scalar: GLfloat; const v: TVec3): TVec3;
    class operator +(const av, bv: TVec3): TVec3;
    class operator -(const v: TVec3): TVec3;
    class operator -(const v: TVec3; const scalar: GLfloat): TVec3;
    class operator -(const av, bv: TVec3): TVec3;
    class operator * (const v: TVec3; const scalar: GLfloat): TVec3;
    class operator * (const scalar: GLfloat; const v: TVec3): TVec3;
    class operator * (const av, bv: TVec3): TVec3;
    class operator / (const v: TVec3; const scalar: GLfloat): TVec3;
    class operator / (const av, bv: TVec3): TVec3;
    // 向量点乘：（内积）
    class operator ** (const av, bv: TVec3): GLfloat;
    // 向量叉乘：（外积）
    class operator >< (const av, bv: TVec3): TVec3;

    case integer of
      0: (x, y, z: GLfloat);
      1: (r, g, b: GLfloat);
      2: (s, t, p: GLfloat);
      3: (Data: array[0..2] of GLfloat);
  end;

  TVec4 = packed record
    constructor Create(v0, v1, v2, v3: GLfloat);
    procedure InitZero;
    procedure InitOne;
    function Length: single;
    function SquaredLength: single;
    class operator +(const v: TVec4; const scalar: GLfloat): TVec4;
    class operator +(const scalar: GLfloat; const v: TVec4): TVec4;
    class operator +(const av, bv: TVec4): TVec4;
    class operator -(const v: TVec4): TVec4;
    class operator -(const v: TVec4; const scalar: GLfloat): TVec4;
    class operator -(const av, bv: TVec4): TVec4;
    class operator * (const v: TVec4; const scalar: GLfloat): TVec4;
    class operator * (const scalar: GLfloat; const v: TVec4): TVec4;
    class operator * (const av, bv: TVec4): TVec4;
    class operator / (const v: TVec4; const scalar: GLfloat): TVec4;
    class operator / (const av, bv: TVec4): TVec4;
    // 向量点乘：（内积）
    class operator ** (const av, bv: TVec4): GLfloat;

    case integer of
      0: (x, y, z, w: GLfloat);
      1: (r, g, b, a: GLfloat);
      2: (s, t, p, q: GLfloat);
      3: (Data: array[0..3] of GLfloat);
  end;


implementation

{ TVec2 }

class operator TVec2. * (const av, bv: TVec2): TVec2;
begin
  with Result do
  begin
    x := av.x * bv.x;
    y := av.y * bv.y;
  end;
end;

class operator TVec2. * (const scalar: GLfloat; const v: TVec2): TVec2;
begin
  Result := v * scalar;
end;

class operator TVec2. * (const v: TVec2; const scalar: GLfloat): TVec2;
begin
  with Result do
  begin
    x := v.x * scalar;
    y := v.y * scalar;
  end;
end;

class operator TVec2. ** (const av, bv: TVec2): GLfloat;
begin
  Result := av.x * bv.x + av.y * bv.y;
end;

class operator TVec2. +(const av, bv: TVec2): TVec2;
begin
  with Result do
  begin
    x := av.x + bv.x;
    y := av.y + bv.y;
  end;
end;

class operator TVec2. +(const scalar: GLfloat; const v: TVec2): TVec2;
begin
  Result := v + scalar;
end;

class operator TVec2. +(const v: TVec2; const scalar: GLfloat): TVec2;
begin
  with Result do
  begin
    x := v.x + scalar;
    y := v.y + scalar;
  end;
end;

class operator TVec2. -(const av, bv: TVec2): TVec2;
begin
  Result := av + -bv;
end;

class operator TVec2. -(const v: TVec2): TVec2;
begin
  with Result do
  begin
    x := -v.x;
    y := -v.y;
  end;
end;

class operator TVec2. -(const v: TVec2; const scalar: GLfloat): TVec2;
begin
  with Result do
  begin
    x := v.x - scalar;
    y := v.y - scalar;
  end;
end;

class operator TVec2. / (const av, bv: TVec2): TVec2;
begin
  with Result do
  begin
    x := av.x / bv.x;
    y := av.y / bv.y;
  end;
end;

class operator TVec2. / (const v: TVec2; const scalar: GLfloat): TVec2;
begin
  with Result do
  begin
    x := v.x / scalar;
    y := v.y / scalar;
  end;
end;

constructor TVec2.Create(v0, v1: GLfloat);
begin
  x := v0;
  y := v1;
end;

procedure TVec2.InitOne;
begin
  Create(1, 1);
end;

procedure TVec2.InitZero;
begin
  Create(0, 0);
end;

function TVec2.Length: single;
begin
  Result := sqrt(x * x + y * y);
end;

function TVec2.SquaredLength: single;
begin
  Result := x * x + y * y;
end;

{ TVec3 }

class operator TVec3. * (const av, bv: TVec3): TVec3;
begin
  with Result do
  begin
    x := av.x * bv.x;
    y := av.y * bv.y;
    z := av.z * bv.z;
  end;
end;

class operator TVec3. * (const scalar: GLfloat; const v: TVec3): TVec3;
begin
  Result := v * scalar;
end;

class operator TVec3. * (const v: TVec3; const scalar: GLfloat): TVec3;
begin
  with Result do
  begin
    x := v.x * scalar;
    y := v.y * scalar;
    z := v.z * scalar;
  end;
end;

class operator TVec3. ** (const av, bv: TVec3): GLfloat;
begin
  Result := av.x * bv.x + av.y * bv.y + av.z * bv.z;
end;

class operator TVec3. +(const av, bv: TVec3): TVec3;
begin
  with Result do
  begin
    x := av.x + bv.x;
    y := av.y + bv.y;
    z := av.z + bv.z;
  end;
end;

class operator TVec3. +(const scalar: GLfloat; const v: TVec3): TVec3;
begin
  Result := v + scalar;
end;

class operator TVec3. +(const v: TVec3; const scalar: GLfloat): TVec3;
begin
  with Result do
  begin
    x := v.x + scalar;
    y := v.y + scalar;
    z := v.z + scalar;
  end;
end;

class operator TVec3. -(const av, bv: TVec3): TVec3;
begin
  Result := av + -bv;
end;

class operator TVec3. -(const v: TVec3): TVec3;
begin
  with Result do
  begin
    x := -v.x;
    y := -v.y;
    z := -v.z;
  end;
end;

class operator TVec3. -(const v: TVec3; const scalar: GLfloat): TVec3;
begin
  with Result do
  begin
    x := v.x - scalar;
    y := v.y - scalar;
    z := v.z - scalar;
  end;
end;

class operator TVec3. / (const av, bv: TVec3): TVec3;
begin
  with Result do
  begin
    x := av.x / bv.x;
    y := av.y / bv.y;
    z := av.z / bv.z;
  end;
end;

class operator TVec3. / (const v: TVec3; const scalar: GLfloat): TVec3;
begin
  with Result do
  begin
    x := v.x / scalar;
    y := v.y / scalar;
    z := v.z / scalar;
  end;
end;

class operator TVec3. >< (const av, bv: TVec3): TVec3;
begin
  with Result do
  begin
    x := av.y * bv.z - av.z * bv.y;
    y := av.z * bv.x - av.x * bv.z;
    z := av.x * bv.y - av.y * bv.x;
  end;
end;

constructor TVec3.Create(v0, v1, v2: GLfloat);
begin
  x := v0;
  y := v1;
  z := v2;
end;

procedure TVec3.InitOne;
begin
  Create(1, 1, 1);
end;

procedure TVec3.InitZero;
begin
  Create(0, 0, 0);
end;

function TVec3.Length: single;
begin
  Result := sqrt(x * x + y * y + z * z);
end;

function TVec3.SquaredLength: single;
begin
  Result := x * x + y * y + z * z;
end;

{ TVec4 }

class operator TVec4. * (const av, bv: TVec4): TVec4;
begin
  with Result do
  begin
    x := av.x * bv.x;
    y := av.y * bv.y;
    z := av.z * bv.z;
    w := av.w * bv.w;
  end;
end;

class operator TVec4. * (const scalar: GLfloat; const v: TVec4): TVec4;
begin
  Result := v * scalar;
end;

class operator TVec4. * (const v: TVec4; const scalar: GLfloat): TVec4;
begin
  with Result do
  begin
    x := v.x / scalar;
    y := v.y / scalar;
    z := v.z / scalar;
    w := v.w / scalar;
  end;
end;

class operator TVec4. ** (const av, bv: TVec4): GLfloat;
begin
  Result := av.x * bv.x + av.y * bv.y + av.z * bv.z + av.w * bv.w;
end;

class operator TVec4. +(const av, bv: TVec4): TVec4;
begin
  with Result do
  begin
    x := av.x + bv.x;
    y := av.y + bv.y;
    z := av.z + bv.z;
    w := av.w + bv.w;
  end;
end;

class operator TVec4. +(const scalar: GLfloat; const v: TVec4): TVec4;
begin
  Result := v + scalar;
end;

class operator TVec4. +(const v: TVec4; const scalar: GLfloat): TVec4;
begin
  with Result do
  begin
    x := v.x + scalar;
    y := v.y + scalar;
    z := v.z + scalar;
    w := v.w + scalar;
  end;
end;

class operator TVec4. -(const av, bv: TVec4): TVec4;
begin
  with Result do
  begin
    x := av.x - bv.x;
    y := av.y - bv.y;
    z := av.z - bv.z;
    w := av.w - bv.w;
  end;
end;

class operator TVec4. -(const v: TVec4): TVec4;
begin
  with Result do
  begin
    x := -v.x;
    y := -v.y;
    z := -v.z;
    w := -v.w;
  end;
end;

class operator TVec4. -(const v: TVec4; const scalar: GLfloat): TVec4;
begin
  with Result do
  begin
    x := v.x - scalar;
    y := v.y - scalar;
    z := v.z - scalar;
    w := v.w - scalar;
  end;
end;

class operator TVec4. / (const av, bv: TVec4): TVec4;
begin
  with Result do
  begin
    x := av.x / bv.x;
    y := av.y / bv.y;
    z := av.z / bv.z;
    w := av.w / bv.w;
  end;
end;

class operator TVec4. / (const v: TVec4; const scalar: GLfloat): TVec4;
begin
  with Result do
  begin
    x := v.x / scalar;
    y := v.y / scalar;
    z := v.z / scalar;
    w := v.w / scalar;
  end;
end;

constructor TVec4.Create(v0, v1, v2, v3: GLfloat);
begin
  x := v0;
  y := v1;
  z := v2;
  w := v3;
end;

procedure TVec4.InitOne;
begin
  Create(1, 1, 1, 1);
end;

procedure TVec4.InitZero;
begin
  Create(0, 0, 0, 0);
end;

function TVec4.Length: single;
begin
  Result := sqrt(x * x + y * y + z * z + w * w);
end;

function TVec4.SquaredLength: single;
begin
  Result := x * x + y * y + z * z + w * w;
end;

end.
