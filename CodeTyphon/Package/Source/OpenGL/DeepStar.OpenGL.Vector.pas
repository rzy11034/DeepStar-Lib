unit DeepStar.OpenGL.Vector;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}{$J-}

interface

uses
  Classes,
  SysUtils;

type
  TVec2 = packed record
    constructor Create(v0, v1: single);
    procedure Init_Zero;
    procedure Init_One;
    function Length: single;
    function SquaredLength: single;

    class operator +(const vec: TVec2; const scalar: single): TVec2;
    class operator +(const scalar: single; const vec: TVec2): TVec2;
    class operator +(const vec1, vec2: TVec2): TVec2;

    class operator -(const vec: TVec2): TVec2;
    class operator -(const vec: TVec2; const scalar: single): TVec2;
    class operator -(const vec1, vec2: TVec2): TVec2;

    class operator * (const vec: TVec2; const scalar: single): TVec2;
    class operator * (const scalar: single; const vec: TVec2): TVec2;
    class operator * (const vec1, vec2: TVec2): TVec2;

    class operator / (const vec: TVec2; const scalar: single): TVec2;
    class operator / (const vec1, vec2: TVec2): TVec2;

    // 向量点乘：（内积）
    class operator ** (const vec1, vec2: TVec2): single;

    case integer of
      0: (v: array[0..1] of single);
      1: (x, y: single);
  end;

  TVec3 = packed record
    constructor Create(v0, v1, v2: single);
    procedure Init_Zero;
    procedure Init_One;
    function Length: single;
    function SquaredLength: single;
    class operator +(const  vec: TVec3; const scalar: single): TVec3;
    class operator +(const scalar: single; const vec: TVec3): TVec3;
    class operator +(const vec1, vec2: TVec3): TVec3;
    class operator -(const vec: TVec3): TVec3;
    class operator -(const vec: TVec3; const scalar: single): TVec3;
    class operator -(const vec1, vec2: TVec3): TVec3;
    class operator * (const vec: TVec3; const scalar: single): TVec3;
    class operator * (const scalar: single; const vec: TVec3): TVec3;
    class operator * (const vec1, vec2: TVec3): TVec3;
    class operator / (const vec: TVec3; const scalar: single): TVec3;
    class operator / (const vec1, vec2: TVec3): TVec3;
    // 向量点乘：（内积）
    class operator ** (const vec1, vec2: TVec3): single;
    // 向量叉乘：（外积）
    class operator >< (const vec1, vec2: TVec3): TVec3;

    case integer of
      0: (v: array[0..2] of single);
      1: (x, y, z: single);
      2: (r, g, b: single);
      3: (s, t, p: single);
  end;

  TVec4 = packed record
    constructor Create(v0, v1, v2, v3: single);
    procedure Init_Zero;
    procedure Init_One;
    function Length: single;
    function SquaredLength: single;

    class operator +(const vec: TVec4; const scalar: single): TVec4;
    class operator +(const scalar: single; const vec: TVec4): TVec4;
    class operator +(const vec1, vec2: TVec4): TVec4;

    class operator -(const vec: TVec4): TVec4;
    class operator -(const vec: TVec4; const scalar: single): TVec4;
    class operator -(const vec1, vec2: TVec4): TVec4;

    class operator * (const vec: TVec4; const scalar: single): TVec4;
    class operator * (const scalar: single; const vec: TVec4): TVec4;
    class operator * (const vec1, vec2: TVec4): TVec4;

    class operator / (const vec: TVec4; const scalar: single): TVec4;
    class operator / (const vec1, vec2: TVec4): TVec4;

    // 向量点乘：（内积）
    class operator ** (const vec1, vec2: TVec4): single;

    case integer of
      0: (v: array[0..3] of single);
      1: (x, y, z, w: single);
      2: (r, g, b, a: single);
      3: (s, t, p, q: single);
  end;

implementation

{ TVec2 }

class operator TVec2. * (const vec1, vec2: TVec2): TVec2;
begin
  with Result do
  begin
    x := vec1.x * vec2.x;
    y := vec1.y * vec2.y;
  end;
end;

class operator TVec2.*(const scalar: single; const vec: TVec2): TVec2;
begin
  Result := vec * scalar;
end;

class operator TVec2.*(const vec: TVec2; const scalar: single): TVec2;
begin
  with Result do
  begin
    x := vec.x * scalar;
    y := vec.y * scalar;
  end;
end;

class operator TVec2. ** (const vec1, vec2: TVec2): single;
begin
  Result := vec1.x * vec2.x + vec1.y * vec2.y;
end;

class operator TVec2. +(const vec1, vec2: TVec2): TVec2;
begin
  with Result do
  begin
    x := vec1.x + vec2.x;
    y := vec1.y + vec2.y;
  end;
end;

class operator TVec2.+(const scalar: single; const vec: TVec2): TVec2;
begin
  Result := vec + scalar;
end;

class operator TVec2.+(const vec: TVec2; const scalar: single): TVec2;
begin
  with Result do
  begin
    x := vec.x + scalar;
    y := vec.y + scalar;
  end;
end;

class operator TVec2. -(const vec1, vec2: TVec2): TVec2;
begin
  Result := vec1 + -vec2;
end;

class operator TVec2.-(const vec: TVec2): TVec2;
begin
  with Result do
  begin
    x := -vec.x;
    y := -vec.y;
  end;
end;

class operator TVec2.-(const vec: TVec2; const scalar: single): TVec2;
begin
  with Result do
  begin
    x := vec.x - scalar;
    y := vec.y - scalar;
  end;
end;

class operator TVec2. / (const vec1, vec2: TVec2): TVec2;
begin
  with Result do
  begin
    x := vec1.x / vec2.x;
    y := vec1.y / vec2.y;
  end;
end;

class operator TVec2./(const vec: TVec2; const scalar: single): TVec2;
begin
  with Result do
  begin
    x := vec.x / scalar;
    y := vec.y / scalar;
  end;
end;

constructor TVec2.Create(v0, v1: single);
begin
  x := v0;
  y := v1;
end;

procedure TVec2.Init_One;
begin
  Create(1, 1);
end;

procedure TVec2.Init_Zero;
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

class operator TVec3. * (const vec1, vec2: TVec3): TVec3;
begin
  with Result do
  begin
    x := vec1.x * vec2.x;
    y := vec1.y * vec2.y;
    z := vec1.z * vec2.z;
  end;
end;

class operator TVec3.*(const scalar: single; const vec: TVec3): TVec3;
begin
  Result := vec * scalar;
end;

class operator TVec3.*(const vec: TVec3; const scalar: single): TVec3;
begin
  with Result do
  begin
    x := vec.x * scalar;
    y := vec.y * scalar;
    z := vec.z * scalar;
  end;
end;

class operator TVec3. ** (const vec1, vec2: TVec3): single;
begin
  Result := vec1.x * vec2.x + vec1.y * vec2.y + vec1.z * vec2.z;
end;

class operator TVec3. +(const vec1, vec2: TVec3): TVec3;
begin
  with Result do
  begin
    x := vec1.x + vec2.x;
    y := vec1.y + vec2.y;
    z := vec1.z + vec2.z;
  end;
end;

class operator TVec3.+(const scalar: single; const vec: TVec3): TVec3;
begin
  Result := vec + scalar;
end;

class operator TVec3.+(const vec: TVec3; const scalar: single): TVec3;
begin
  with Result do
  begin
    x := vec.x + scalar;
    y := vec.y + scalar;
    z := vec.z + scalar;
  end;
end;

class operator TVec3. -(const vec1, vec2: TVec3): TVec3;
begin
  Result := vec1 + -vec2;
end;

class operator TVec3.-(const vec: TVec3): TVec3;
begin
  with Result do
  begin
    x := -vec.x;
    y := -vec.y;
    z := -vec.z;
  end;
end;

class operator TVec3.-(const vec: TVec3; const scalar: single): TVec3;
begin
  with Result do
  begin
    x := vec.x - scalar;
    y := vec.y - scalar;
    z := vec.z - scalar;
  end;
end;

class operator TVec3. / (const vec1, vec2: TVec3): TVec3;
begin
  with Result do
  begin
    x := vec1.x / vec2.x;
    y := vec1.y / vec2.y;
    z := vec1.z / vec2.z;
  end;
end;

class operator TVec3./(const vec: TVec3; const scalar: single): TVec3;
begin
  with Result do
  begin
    x := vec.x / scalar;
    y := vec.y / scalar;
    z := vec.z / scalar;
  end;
end;

class operator TVec3. >< (const vec1, vec2: TVec3): TVec3;
begin
  with Result do
  begin
    x := vec1.y * vec2.z - vec1.z * vec2.y;
    y := vec1.z * vec2.x - vec1.x * vec2.z;
    z := vec1.x * vec2.y - vec1.y * vec2.x;
  end;
end;

constructor TVec3.Create(v0, v1, v2: single);
begin
  x := v0;
  y := v1;
  z := v2;
end;

procedure TVec3.Init_One;
begin
  Create(1, 1, 1);
end;

procedure TVec3.Init_Zero;
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

class operator TVec4. * (const vec1, vec2: TVec4): TVec4;
begin
  with Result do
  begin
    x := vec1.x * vec2.x;
    y := vec1.y * vec2.y;
    z := vec1.z * vec2.z;
    w := vec1.w * vec2.w;
  end;
end;

class operator TVec4.*(const scalar: single; const vec: TVec4): TVec4;
begin
  Result := vec * scalar;
end;

class operator TVec4.*(const vec: TVec4; const scalar: single): TVec4;
begin
  with Result do
  begin
    x := vec.x * scalar;
    y := vec.y * scalar;
    z := vec.z * scalar;
    w := vec.w * scalar;
  end;
end;

class operator TVec4. ** (const vec1, vec2: TVec4): single;
begin
  Result := vec1.x * vec2.x + vec1.y * vec2.y + vec1.z * vec2.z + vec1.w * vec2.w;
end;

class operator TVec4. +(const vec1, vec2: TVec4): TVec4;
begin
  with Result do
  begin
    x := vec1.x + vec2.x;
    y := vec1.y + vec2.y;
    z := vec1.z + vec2.z;
    w := vec1.w + vec2.w;
  end;
end;

class operator TVec4.+(const scalar: single; const vec: TVec4): TVec4;
begin
  Result := vec + scalar;
end;

class operator TVec4.+(const vec: TVec4; const scalar: single): TVec4;
begin
  with Result do
  begin
    x := vec.x + scalar;
    y := vec.y + scalar;
    z := vec.z + scalar;
    w := vec.w + scalar;
  end;
end;

class operator TVec4. -(const vec1, vec2: TVec4): TVec4;
begin
  with Result do
  begin
    x := vec1.x - vec2.x;
    y := vec1.y - vec2.y;
    z := vec1.z - vec2.z;
    w := vec1.w - vec2.w;
  end;
end;

class operator TVec4.-(const vec: TVec4): TVec4;
begin
  with Result do
  begin
    x := -vec.x;
    y := -vec.y;
    z := -vec.z;
    w := -vec.w;
  end;
end;

class operator TVec4.-(const vec: TVec4; const scalar: single): TVec4;
begin
  with Result do
  begin
    x := vec.x - scalar;
    y := vec.y - scalar;
    z := vec.z - scalar;
    w := vec.w - scalar;
  end;
end;

class operator TVec4. / (const vec1, vec2: TVec4): TVec4;
begin
  with Result do
  begin
    x := vec1.x / vec2.x;
    y := vec1.y / vec2.y;
    z := vec1.z / vec2.z;
    w := vec1.w / vec2.w;
  end;
end;

class operator TVec4./(const vec: TVec4; const scalar: single): TVec4;
begin
  with Result do
  begin
    x := vec.x / scalar;
    y := vec.y / scalar;
    z := vec.z / scalar;
    w := vec.w / scalar;
  end;
end;

constructor TVec4.Create(v0, v1, v2, v3: single);
begin
  x := v0;
  y := v1;
  z := v2;
  w := v3;
end;

procedure TVec4.Init_One;
begin
  Create(1, 1, 1, 1);
end;

procedure TVec4.Init_Zero;
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
