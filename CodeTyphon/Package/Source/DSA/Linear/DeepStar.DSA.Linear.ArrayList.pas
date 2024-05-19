unit DeepStar.DSA.Linear.ArrayList;

{$mode objfpc}{$H+}
{$ModeSwitch unicodestrings}{$J-}

interface

uses
  Classes,
  SysUtils,
  Rtti,
  DeepStar.DSA.Interfaces;

type
  generic TArrayList<T> = class(TInterfacedObject, specialize IList<T>)
  public type
    TImpl = specialize TImpl<T>;
    TArr = TImpl.TArr;
    ICmp = TImpl.ICmp;
    TCmp = TImpl.TCmp;
    PT = ^T;

  private
    _Data: TArr;
    _Capacity: uint64;
    _Size: integer;
    _Cmp: TImpl.ICmp;

    function __GetItemPtr(index: integer): PT;
    procedure __quickSort(l, r: integer);
    procedure __ResizeIncreases;
    procedure __ResizeDecreases;
    procedure __SetComparer(const newComparer: TImpl.ICmp);
    procedure __Swap(var a, b: T);

  public
    // 构造函数，传入数组的容量 capacity 构造 TArrayList
    // 默认数组的容量capacity:=10
    constructor Create(capacity: integer = 4);
    // 构造函数，传入数组构造 TArrayList
    constructor Create(const arr: TArr);
    // 构造函数，传入 TComparisonFunc。
    constructor Create(comparisonFunc: TImpl.TComparisonFuncs);
    // 构造函数，传入 TOnComparisons。
    constructor Create(onComparison: TImpl.TOnComparisons);
    // 构造函数，传入 IComparer
    constructor Create(cmp: TImpl.ICmp);
    destructor Destroy; override;

    // 获取数组中的元数个数
    function GetSize: integer;
    // 获取数组的容量
    function GetCapacity: integer;
    // 返回数组是否有空
    function IsEmpty: boolean;
    // 获取index索引位置元素
    function GetItem(index: integer): T;
    // 获取第一个元素
    function GetFirst: T;
    // 获取最后一个元素
    function GetLast: T;
    // 修改index索引位置元素
    procedure SetItem(index: integer; e: T);
    // 向所有元素后添加一个新元素
    procedure AddLast(e: T);
    // 在第index个位置插入一个新元素e
    procedure Add(index: integer; e: T);
    // 在所有元素前添加一个新元素
    procedure AddFirst(e: T);
    // 添加数组所有元素
    procedure AddRange(const arr: array of T);
    // 添加数组aIndex开始共aCount个元素
    procedure AddRange(const arr: array of T; aIndex, aCount: integer);
    // 查找数组中是否有元素e
    function Contains(e: T): boolean;
    // 查找数组中元素e忆的索引，如果不存在元素e，则返回-1
    function IndexOf(e: T): integer;
    // 从数组中删除index位置的元素，返回删除的元素
    function Remove(index: integer): T;
    // 从数组中删除第一个元素，返回删除的元素
    function RemoveFirst: T;
    // 从数组中删除i最后一个元素，返回删除的元素
    function RemoveLast: T;
    // 从数组中删除元素e
    procedure RemoveElement(e: T);
    // 排序
    procedure Sort;
    // 返回一个数组
    function ToArray: TArr;
    // 清空列表
    procedure Clear;
    // 反转列表
    procedure Reverse;

    function ToString: string; reintroduce;

    property Count: integer read GetSize;
    property Comparer: TImpl.ICmp write __SetComparer;
    property Items[i: integer]: T read GetItem write SetItem; default;
    property ItemPtr[i: integer]: PT read __GetItemPtr;
  end;

implementation

{ TArrayList }

procedure TArrayList.Add(index: integer; e: T);
var
  i: integer;
begin
  if (index < 0) or (index > _Size) then
    raise Exception.Create('Add failed. Require index >= 0 and index <= Size.');

  if (_Size = Length(_Data)) then
    __ResizeIncreases;

  for i := _Size - 1 downto index do
    _Data[i + 1] := _Data[i];

  _Data[index] := e;
  _Size += 1;
end;

procedure TArrayList.AddFirst(e: T);
begin
  Add(0, e);
end;

procedure TArrayList.AddLast(e: T);
begin
  Add(_Size, e);
end;

procedure TArrayList.AddRange(const arr: array of T);
begin
  AddRange(arr, 0, Length(arr));
end;

procedure TArrayList.AddRange(const arr: array of T; aIndex, aCount: integer);
var
  i: integer;
begin
  if (aIndex < 0) or (aIndex + aCount > Length(arr)) then
    raise Exception.Create('Add failed.');

  for i := aIndex to aCount - 1 do
  begin
    Self.AddLast(arr[i]);
  end;
end;

procedure TArrayList.Clear;
begin
  _Size := 0;
  _Data := nil;
  SetLength(_Data, 10);
end;

function TArrayList.Contains(e: T): boolean;
var
  i: integer;
begin
  for i := 0 to _Size - 1 do
  begin
    if _Cmp.Compare(_Data[i], e) = 0 then
      Exit(true);
  end;

  Result := false;
end;

destructor TArrayList.Destroy;
begin
  inherited Destroy;
end;

constructor TArrayList.Create(capacity: integer);
begin
  if capacity < 4 then capacity := 4;


  SetLength(_Data, capacity);
  _Cmp := TImpl.TCmp.Default;

  _Size := 0;
  _Capacity := capacity;
end;

constructor TArrayList.Create(const arr: TArr);
var
  i: integer;
begin
  SetLength(_Data, Length(arr));

  for i := 0 to Length(arr) - 1 do
    _Data[i] := arr[i];

  _Size := Length(arr);
  _Capacity := _Size;
end;

constructor TArrayList.Create(cmp: TImpl.ICmp);
begin
  Self.Create;
  _Cmp := cmp;
end;

constructor TArrayList.Create(comparisonFunc: TImpl.TComparisonFuncs);
begin
  Self.Create;
  _Cmp := TImpl.TCmp.Construct(ComparisonFunc);
end;

constructor TArrayList.Create(onComparison: TImpl.TOnComparisons);
begin
  Self.Create;
  _Cmp := TImpl.TCmp.Construct(OnComparison);
end;

function TArrayList.IndexOf(e: T): integer;
var
  i: integer;
begin
  for i := 0 to _Size - 1 do
  begin
    if _Cmp.Compare(_Data[i], e) = 0 then
      Exit(i);
  end;

  Result := -1;
end;

function TArrayList.GetItem(index: integer): T;
begin
  if (index < 0) or (index >= _Size) then
    raise Exception.Create('Get failed. Index is illegal.');

  Result := _Data[index];
end;

function TArrayList.GetCapacity: integer;
begin
  Result := Length(Self._Data);
end;

function TArrayList.GetFirst: T;
begin
  Result := GetItem(0);
end;

function TArrayList.GetLast: T;
begin
  Result := GetItem(_Size - 1);
end;

function TArrayList.GetSize: integer;
begin
  Result := Self._Size;
end;

function TArrayList.IsEmpty: boolean;
begin
  Result := Self._Size = 0;
end;

function TArrayList.Remove(index: integer): T;
var
  i: integer;
  res: T;
begin
  if (index < 0) or (index >= _Size) then
    raise Exception.Create('Remove failed. Index is illegal.');

  res := _Data[index];

  for i := index + 1 to _Size - 1 do
    _Data[i - 1] := _Data[i];

  Dec(Self._Size);

  __ResizeDecreases;

  Result := res;
end;

procedure TArrayList.RemoveElement(e: T);
var
  index, i: integer;
begin
  for i := 0 to _Size - 1 do
  begin
    index := IndexOf(e);

    if index <> -1 then
      Remove(index);
  end;
end;

function TArrayList.RemoveFirst: T;
begin
  Result := Remove(0);
end;

function TArrayList.RemoveLast: T;
begin
  Result := Remove(_Size - 1);
end;

procedure TArrayList.Reverse;
var
  l, r: integer;
begin
  l := 0;
  r := _Size - 1;

  while l < r do
  begin
    __Swap(_Data[l], _Data[r]);
    l += 1;
    r -= 1;
  end;
end;

procedure TArrayList.SetItem(index: integer; e: T);
begin
  if (index < 0) or (index >= _Size) then
    raise Exception.Create('Set failed. Require index >= 0 and index < Size.');

  _Data[index] := e;
end;

procedure TArrayList.Sort;
begin
  __quickSort(0, _Size - 1);
end;

function TArrayList.ToArray: TArr;
var
  i: integer;
  arr: TArr;
begin
  SetLength(arr, _Size);

  for i := 0 to _Size - 1 do
    arr[i] := _Data[i];

  Result := arr;
end;

function TArrayList.ToString: string;
var
  res: TStringBuilder;
  i: integer;
  Value: TValue;
begin
  res := TStringBuilder.Create;
  try
    res.AppendFormat('Array: Size = %d, capacity = %d',
      [Self._Size, Length(Self._Data)]);
    res.AppendLine;
    res.Append('  [');

    for i := 0 to _Size - 1 do
    begin
      TValue.Make(@_Data[i], TypeInfo(T), Value);

      if not (Value.IsObject) then
        res.Append(Value.ToString)
      else
        res.Append(Value.AsObject.ToString);

      if i <> _Size - 1 then
        res.Append(', ');
    end;

    res.Append(']');
    Result := res.ToString;

  finally
    res.Free;
  end;
end;

function TArrayList.__GetItemPtr(index: integer): PT;
begin
  if (index < 0) or (index >= _Size) then
    raise Exception.Create('Set failed. Require index >= 0 and index < Size.');

  Result := @_Data[index];
end;

procedure TArrayList.__quickSort(l, r: integer);
var
  i, j: integer;
  p, q: T;
begin
  if ((r - l) <= 0) or (Length(_Data) = 0) then
    Exit;

  repeat
    i := l;
    j := r;
    p := _Data[l + (r - l) shr 1];
    repeat
      while _Cmp.Compare(_Data[i], p) < 0 do
        Inc(i);
      while _Cmp.Compare(_Data[j], p) > 0 do
        Dec(j);
      if i <= j then
      begin
        if i <> j then
        begin
          q := _Data[i];
          _Data[i] := _Data[j];
          _Data[j] := q;
        end;
        Inc(i);
        Dec(j);
      end;
    until i > j;
    // sort the smaller range recursively
    // sort the bigger range via the loop
    // Reasons: memory usage is O(log(n)) instead of O(n) and loop is faster than recursion
    if j - l < r - i then
    begin
      if l < j then
        __quickSort(l, j);
      l := i;
    end
    else
    begin
      if i < r then
        __quickSort(i, r);
      r := j;
    end;
  until l >= r;
end;

procedure TArrayList.__ResizeDecreases;
begin
  if _Capacity >= 20 then
  begin
    if (_Size = Length(Self._Data) div 4) and (Length(Self._Data) div 2 <> 0) then
    begin
      SetLength(Self._Data, Length(Self._Data) div 4);
      _Capacity := _Size;
    end;
  end
  else
    Exit;
end;

procedure TArrayList.__ResizeIncreases;
  function __NewCapacity__: SizeUInt;
  const
    // if size is small, multiply by 2;
    // if size bigger but <256M, inc by 1/8*size;
    // otherwise inc by 1/16*size
    cSizeSmall = 1 * 1024 * 1024;
    cSizeBig = 256 * 1024 * 1024;
  var
    dataSize: SizeUInt;
    res: SizeUInt;
  begin
    dataSize := _Capacity * SizeOf(T);

    if _Capacity = 0 then
      res := 4
    else if dataSize < cSizeSmall then
      res := _Capacity * 2
    else if dataSize < cSizeBig then
      res := _Capacity + _Capacity div 8
    else
      res := _Capacity + _Capacity div 16;

    Result := res;
  end;

begin
  _Capacity := __NewCapacity__;
  SetLength(Self._Data, _Capacity);
end;

procedure TArrayList.__SetComparer(const newComparer: TImpl.ICmp);
begin
  _Cmp := newComparer;
end;

procedure TArrayList.__Swap(var a, b: T);
var
  temp: T;
begin
  temp := a;
  a := b;
  b := temp;
end;

end.
