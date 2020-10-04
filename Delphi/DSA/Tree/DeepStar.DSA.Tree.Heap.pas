unit DeepStar.DSA.Tree.Heap;

interface

uses
  System.SysUtils,
  DeepStar.DSA.Interfaces,
  DeepStar.DSA.Linear.ArrayList;

type
  THeapkind = (Min, Max);

  THeap<T> = class
  private type
    TImpl = TImpl<T>;
    TList = TArrayList<T>;
    TArr = TImpl.TArr;
    ICmp = TImpl.ICmp;

  private
    _data: TList;
    _cmp: ICmp;
    _heapKind: THeapkind;

    // 返回完全二叉树的数组表示中，一个索引所表示的元素的父亲节点的索引
    function __parent(index: integer): integer;
    // 返回完全二叉树的数组表示中，一个索引所表示的元素的左孩子节点的索引
    function __leftChild(index: integer): integer;
    // 返回完全二叉树的数组表示中，一个索引所表示的元素的右孩子节点的索引
    function __rightChild(index: integer): integer;
    // 交换索引为 i ， j 的节点元素
    procedure __swap(i: integer; j: integer);
    // 元素上浮过程
    procedure __shiftUp(k: integer);
    // 元素下沉过程
    procedure __shiftDown(k: integer);

  public
    constructor Create(capacity: integer = 10; heapKind: THeapkind = THeapkind.Min); overload;
    constructor Create(const arr: TArr; cmp: ICmp; heapKind: THeapkind = THeapkind.Min); overload;
    constructor Create(const arr: TArr; heapKind: THeapkind = THeapkind.Min); overload;
    destructor Destroy; override;

    // 返回堆中元素个数 </summary>
    function Count: integer;
    // 返回天所布尔值，表示堆中是否为空 </summary>
    function IsEmpty: boolean;
    // 向堆中添加元素
    procedure Add(e: T);
    // 返回堆中的第一个元素的值
    function FindFirst: T;
    // 取出堆中第一个元素
    function ExtractFirst: T;
    // 替换堆顶元素
    function Replace(e: T): T;

    property Comparer: ICmp read _cmp write _cmp;
  end;

implementation

{ THeap<T> }

constructor THeap<T>.Create(capacity: integer; heapKind: THeapkind);
begin
  _cmp := TImpl.TCmp.Default;
  _data := TList.Create(capacity);
  _heapKind := heapKind;
end;

constructor THeap<T>.Create(const arr: TArr; cmp: ICmp; heapKind: THeapkind);
var
  i: integer;
begin
  _cmp := cmp;
  _data := TList.Create(arr);
  _heapKind := heapKind;

  // heapify
  for i := __parent(_data.GetSize - 1) downto 0 do
  begin
    __shiftDown(i);
  end;
end;

constructor THeap<T>.Create(const arr: TArr; heapKind: THeapkind);
begin
  Self.Create(arr, TImpl.TCmp.Default, heapKind);
end;

procedure THeap<T>.Add(e: T);
begin
  _data.AddLast(e);
  __shiftUp(_data.Count - 1);
end;

function THeap<T>.Count: integer;
begin
  Result := _data.GetSize;
end;

destructor THeap<T>.Destroy;
begin
  _data.Clear;
  FreeAndNil(_data);

  inherited Destroy;
end;

function THeap<T>.ExtractFirst: T;
var
  res: T;
begin
  res := FindFirst;
  __swap(0, _data.GetSize - 1);
  _data.RemoveLast;
  __shiftDown(0);
  Result := res;
end;

function THeap<T>.FindFirst: T;
begin
  if _data.GetSize = 0 then
    raise Exception.Create('Can not findFirst when heap is empty.');

  Result := _data.GetFirst;
end;

function THeap<T>.IsEmpty: boolean;
begin
  Result := _data.IsEmpty;
end;

function THeap<T>.Replace(e: T): T;
var
  res: T;
begin
  if IsEmpty then
  begin
    res := e;
    Add(e);
  end
  else
  begin
    res := _data[0];
    _data[0] := e;
    __shiftDown(0);
  end;

  Result := res;
end;

function THeap<T>.__leftChild(index: integer): integer;
begin
  Result := index * 2 + 1;
end;

function THeap<T>.__parent(index: integer): integer;
begin
  if index = 0 then
    raise Exception.Create('index-0 doesn''t have parent.');

  Result := (index - 1) div 2;
end;

function THeap<T>.__rightChild(index: integer): integer;
begin
  Result := index * 2 + 2;
end;

procedure THeap<T>.__shiftDown(k: integer);
var
  j: integer;
begin
  case _heapKind of
    THeapkind.Min:
      while __leftChild(k) < _data.GetSize do
      begin
        j := __leftChild(k);

        if ((j + 1) < _data.GetSize) and (_cmp.Compare(_data[j + 1], _data[j]) < 0) then
        begin
          j := __rightChild(k); // _data[j] 是 leftChild 和 rightChild 中的最小值
        end;

        if _cmp.Compare(_data[k], _data[j]) <= 0 then
          Break;

        __swap(k, j);
        k := j;
      end;

    THeapkind.Max:
      while __leftChild(k) < _data.GetSize do
      begin
        j := __leftChild(k);

        if ((j + 1) < _data.GetSize) and (_cmp.Compare(_data[j + 1], _data[j]) > 0) then
        begin
          j := __rightChild(k); // _data[j] 是 leftChild 和 rightChild 中的最大值
        end;

        if _cmp.Compare(_data[k], _data[j]) >= 0 then
          Break;

        __swap(k, j);
        k := j;
      end;
  end;
end;

procedure THeap<T>.__shiftUp(k: integer);
begin
  case _heapKind of
    THeapkind.Min:
      while (k > 0) and (_cmp.Compare(_data[__parent(k)], _data[k]) > 0) do
      begin
        __swap(k, __parent(k));
        k := __parent(k);
      end;

    THeapkind.Max:
      while (k > 0) and (_cmp.Compare(_data[__parent(k)], _data[k]) < 0) do
      begin
        __swap(k, __parent(k));
        k := __parent(k);
      end;
  end;
end;

procedure THeap<T>.__swap(i: integer; j: integer);
var
  temp: T;
begin
  if (i < 0) or (i >= Count) or (j < 0) or (j >= Count) then
    raise Exception.Create('index is illegal.');

  temp := _data[i];
  _data[i] := _data[j];
  _data[j] := temp;
end;

end.
