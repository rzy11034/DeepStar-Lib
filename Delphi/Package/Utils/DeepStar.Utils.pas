unit DeepStar.Utils;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Rtti,
  System.Generics.Collections,
  System.Generics.Defaults,
  DeepStar.DSA.Hash.HashMap,
  DeepStar.DSA.Hash.HashSet,
  DeepStar.DSA.Interfaces,
  DeepStar.DSA.Linear.ArrayList,
  DeepStar.DSA.Linear.Queue,
  DeepStar.DSA.Linear.Stack,
  DeepStar.Utils.UString;

type
  TArr_int = TArray<integer>;
  TArr_int64 = TArray<int64>;
  TArr2D_int = TArray<TArr_int>;
  TArr3D_int = TArray<TArr2D_int>;
  TArr_chr = TArray<UChar>;
  TArr2D_chr = TArray<TArr_chr>;
  TArr_str = TArray<UString>;
  TArr_bool = TArray<boolean>;

  TUtils<T> = class
  public
    class procedure Swap(var a, b: T); inline;
  end;

  TUtils_int = TUtils<integer>;
  TUtils_str = TUtils<UString>;
  TUtils_chr = TUtils<UChar>;

  TArrayUtils<T> = class
  private type
    TArr_T = TArray<T>;
    TArr2D_T = TArray<TArray<T>>;
    TArr3D_T = TArray<TArray<TArray<T>>>;

  public
    /// <summary> 快速排序 </summary>
    class procedure Sort(var arr: array of T); overload;
    class procedure Sort(var arr: array of T; const cmp: TComparison<T>); overload;
    /// <summary> 二分查找法 </summary>
    class function BinarySearch(const arr: TArr_T; const e: T): integer; overload;
    class function BinarySearch(const arr: TArr_T; const e: T; const cmp: TComparison<T>): integer; overload;
    /// <summary> 顺序查找，返回元素e的下标，元素不存在则返回 -1 </summary>
    class function IndexOf(const arr: array of T; e: T): integer;
    /// <summary>  输出一维数组 </summary>
    class procedure Print(arr: TArr_T);
    /// <summary> 输出二维数组 </summary>
    class procedure Print2D(arr: TArr2D_T; formated: boolean = True);
    /// <summary> 输出三维数组 </summary>
    class procedure Print3D(arr: TArr3D_T);
    /// <summary> 复制一维数组 </summary>
    class function CopyArray(arr: TArr_T): TArr_T;
    /// <summary> 复制二维数组 </summary>
    class function CopyArray2D(arr2D: TArr2D_T): TArr2D_T;
    /// <summary>  填充数组 </summary>
    class procedure FillArray(var arr: TArr_T; e: T);
  end;

  TArrayUtils_int = TArrayUtils<integer>;
  TArrayUtils_str = TArrayUtils<UString>;
  TArrayUtils_chr = TArrayUtils<UChar>;

type // 容器类
  TList_int = TArrayList<integer>;
  TList_str = TArrayList<UString>;
  TList_chr = TArrayList<UChar>;
  TList_TArr_int = TArrayList<TArr_int>;
  TStack_int = TStack<integer>;
  TStack_chr = TStack<UChar>;
  TMap_int_int = THashMap<integer, integer>;
  TMap_str_int = THashMap<UString, integer>;
  TSet_int = THashSet<integer>;
  TSet_str = THashSet<UString>;

procedure DrawLineBlockEnd;
procedure DrawLineProgramEnd;
procedure NeedInput;
function Chr(i: cardinal): UChar;

resourcestring
  END_OF_PROGRAM_EN = 'Press any key to continue...';
  END_OF_PROGRAM_CH = '按任意键继续...';

implementation

procedure DrawLineBlockEnd;
var
  i: integer;
begin
  for i := 0 to 10 do
  begin
    write('-');
  end;
  Writeln;
end;

procedure DrawLineProgramEnd;
var
  i: integer;
begin
  for i := 0 to 70 do
  begin
    write('=');
  end;
  Writeln;
end;

procedure NeedInput;
begin
  writeln('Need input data: ');
end;

function Chr(i: cardinal): UChar;
begin
  Result := UChar(i);
end;

{ TUtils<T> }

class procedure TUtils<T>.Swap(var a, b: T);
var
  temp: T;
begin
  temp := a;
  a := b;
  b := temp;
end;

{ TArrayUtils<T> }

class function TArrayUtils<T>.BinarySearch(const arr: TArr_T; const e: T;
  const cmp: TComparison<T>): integer;
var
  tmpCmp: IComparer<T>;
  ret: integer;
begin
  Result := -1;
  tmpCmp := TComparer<T>.Construct(cmp);

  if TArray.BinarySearch<T>(arr, e, ret, tmpCmp) then
    Result := ret;
end;

class function TArrayUtils<T>.CopyArray(arr: TArr_T): TArr_T;
begin
  Result := Copy(arr);
end;

class function TArrayUtils<T>.CopyArray2D(arr2D: TArr2D_T): TArr2D_T;
var
  i: integer;
  res: TArr2D_T;
begin
  SetLength(res, Length(arr2D));

  for i := 0 to Length(arr2D) - 1 do
    res[i] := Copy(arr2D[i]);

  Result := res;
end;

class procedure TArrayUtils<T>.FillArray(var arr: TArr_T; e: T);
var
  i: integer;
begin
  for i := 0 to high(arr) do
    arr[i] := e;
end;

class function TArrayUtils<T>.BinarySearch(const arr: TArr_T; const e: T): integer;
var
  ret: integer;
begin
  Result := -1;

  if TArray.BinarySearch<T>(arr, e, ret) then
    Result := ret;
end;

class function TArrayUtils<T>.IndexOf(const arr: array of T; e: T): integer;
var
  cmp: IComparer<T>;
  i: integer;
begin
  Result := -1;
  cmp := TComparer<T>.Default;

  for i := 0 to Length(arr) - 1 do
  begin
    if cmp.Compare(arr[i], e) = 0 then
      Result := i;
  end;
end;

class procedure TArrayUtils<T>.Print(arr: TArr_T);
var
  i: integer;
begin
  if arr = nil then
  begin
    Writeln('[]');
    Exit;
  end;

  write('[');
  for i := 0 to high(arr) do
  begin
    if i <> high(arr) then
      write(TValue.From<T>(arr[i]).ToString, ', ')
    else
      write(TValue.From<T>(arr[i]).ToString);
  end;
  write(']'#10);
end;

class procedure TArrayUtils<T>.Print2D(arr: TArr2D_T; formated: boolean);
var
  i, j: integer;
begin
  if arr = nil then
  begin
    Writeln('[]');
    Exit;
  end;

  case formated of
    True:
      begin
        for i := 0 to high(arr) do
        begin
          write('[');
          for j := 0 to high(arr[i]) do
          begin
            write(TValue.From<T>(arr[i, j]).ToString);

            if j <> high(arr[i]) then
              write(', '#9);
          end;

          write(']'#10);
        end;
      end;

    False:
      begin
        for i := 0 to high(arr) do
        begin
          write('[');
          for j := 0 to high(arr[i]) do
          begin
            write(TValue.From<T>(arr[i, j]).ToString);

            if j <> high(arr[i]) then
              write(', ');
          end;

          write(']'#10);
        end;
      end;
  end;

end;

class procedure TArrayUtils<T>.Print3D(arr: TArr3D_T);
var
  i, j, k: integer;
begin
  if arr = nil then
  begin
    Writeln('[]');
    Exit;
  end;

  for i := 0 to high(arr) do
  begin
    write('[');
    for j := 0 to high(arr[i]) do
    begin
      write('(');
      for k := 0 to high(arr[i, j]) do
      begin
        write(TValue.From<T>(arr[i, j, k]).ToString);

        if k <> high(arr[i, j]) then
          write(',');
      end;
      write(')');

      if j <> high(arr[i]) then
        write(', ');
    end;
    write(']'#10);
  end;
end;

class procedure TArrayUtils<T>.Sort(var arr: array of T; const cmp: TComparison<T>);
var
  tmpCmp: IComparer<T>;
begin
  tmpCmp := TComparer<T>.Construct(cmp);
  TArray.Sort<T>(arr, tmpCmp);
end;

class procedure TArrayUtils<T>.Sort(var arr: array of T);
begin
  TArray.Sort<T>(arr);
end;

end.
