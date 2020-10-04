unit DeepStar.DSA.Tree.TreeMap;

interface

uses
  System.SysUtils,
  System.Rtti,
  DeepStar.DSA.Interfaces,
  DeepStar.DSA.Linear.Queue,
  DeepStar.DSA.Linear.ArrayList;

type
  // 用红黑树实现
  TTreeMap<K, V> = class(TInterfacedObject, IMap<K, V>)
  private const
    RED = false;
    BLACK = true;

  private type
    TNode = class(TObject)
    public
      Key: K;
      Value: V;
      Left: TNode;
      Right: TNode;
      Parent: TNode;
      Color: boolean;

      constructor Create(newKey: K; newValue: V; newParent: TNode);

      // 是否叶子节点
      function IsLeaf: boolean;
      // 是否有两个子节点
      function HasTwoChildren: boolean;
      // 判断自己是不是左子树
      function IsLeftChild: boolean;
      // 判断自己是不是右子树
      function IsRightChild: boolean;
      // 返回兄弟节点
      function Sibling: TNode;
    end;

    TPtr_V = TPtr_V<V>;
    TImpl_K = TImpl<K>;
    TImpl_V = TImpl<V>;
    TList_node = TArrayList<TNode>;
    TQueue_node = TQueue<TNode>;

  private
    _cmp_K: TImpl_K.ICmp;
    _cmp_V: TImpl_V.ICmp;
    _root: TNode;
    _size: integer;

    function __CreateNode(newKey: K; newValue: V; newParent: TNode): TNode;
    function __GetColor(node: TNode): boolean;
    function __getNode(node: TNode; Key: K): TNode;
    function __getSuccessor(Key: K): TNode;
    function __isBlack(node: TNode): boolean;
    function __isRed(node: TNode): boolean;
    function __minNode(node: TNode): TNode;
    function __setBlack(node: TNode): TNode;
    function __setColor(node: TNode; Color: boolean): TNode;
    function __setRed(node: TNode): TNode;
    procedure __afterAdd(node: TNode);
    procedure __afterRemove(node: TNode);
    procedure __afterRotate(grand, Parent, child: TNode);
    procedure __clear(node: TNode);
    procedure __inOrder(node: TNode; list: TList_node);
    procedure __remove(node: TNode);
    procedure __rotateLeft(grand: TNode);
    procedure __rotateRight(grand: TNode);

  public
    constructor Create;
    destructor Destroy; override;

    function Add(Key: K; Value: V): TPtr_V;
    function ContainsKey(Key: K): boolean;
    function ContainsValue(Value: V): boolean;
    function Count: integer;
    function GetItem(Key: K): V;
    function IsEmpty: boolean;
    function Keys: TImpl_K.TArr;
    function Remove(Key: K): TPtr_V;
    function Values: TImpl_V.TArr;
    procedure Clear;
    procedure SetItem(Key: K; Value: V);

    property Comparer_K: TImpl_K.ICmp read _cmp_K write _cmp_K;
    property Comparer_V: TImpl_V.ICmp read _cmp_V write _cmp_V;
    property Item[Key: K]: V read GetItem write SetItem; default;
  end;

implementation

{ TTreeMap<K, V> }

constructor TTreeMap<K, V>.Create;
begin
  _root := nil;
  _size := 0;
  _cmp_K := TImpl_K.TCmp.Default;
  _cmp_V := TImpl_V.TCmp.Default;
end;

function TTreeMap<K, V>.Add(Key: K; Value: V): TPtr_V;
var
  Parent, cur: TNode;
  cmp: integer;
  res: TPtr_V;
begin
  Parent := nil;
  cur := _root;
  cmp := 0;
  res := nil;

  while cur <> nil do
  begin
    Parent := cur;
    cmp := _cmp_K.Compare(Key, cur.Key);

    if cmp < 0 then
      cur := cur.Left
    else if cmp > 0 then
      cur := cur.Right
    else
    begin
      res := TPtr_V.Create(cur.Value);
      cur.Value := Value;
      Exit;
    end;
  end;

  cur := __CreateNode(Key, Value, Parent);
  if Parent = nil then
  begin
    _root := cur;
    __afterAdd(cur);
  end
  else if cmp < 0 then
  begin
    Parent.Left := cur;
    __afterAdd(cur);
  end
  else if cmp > 0 then
  begin
    Parent.Right := cur;
    __afterAdd(cur);
  end;

  _size := _size + 1;
  Result := res;
end;

procedure TTreeMap<K, V>.Clear;
begin
  __clear(_root);
  _root := nil;
  _size := 0;
end;

function TTreeMap<K, V>.ContainsKey(Key: K): boolean;
var
  cur: TNode;
  cmp: integer;
begin
  cur := _root;

  while cur <> nil do
  begin
    cmp := _cmp_K.Compare(Key, cur.Key);
    if cmp < 0 then
      cur := cur.Left
    else if cmp > 0 then
      cur := cur.Right
    else
      Exit(true);
  end;

  Result := false;
end;

function TTreeMap<K, V>.ContainsValue(Value: V): boolean;
var
  Queue: TQueue_node;
  cur: TNode;
begin
  if _root = nil then
    Exit(false);

  cur := _root;

  Queue := TQueue_node.Create;
  try
    Queue.EnQueue(cur);

    while not Queue.IsEmpty do
    begin
      cur := Queue.DeQueue;

      if _cmp_V.Compare(Value, cur.Value) = 0 then
      begin
        Result := true;
        Exit;
      end;

      if cur.Left <> nil then
        Queue.EnQueue(cur.Left);
      if cur.Right <> nil then
        Queue.EnQueue(cur.Right);
    end;

    Result := false;
  finally
    Queue.Free;
  end;
end;

function TTreeMap<K, V>.Count: integer;
begin
  Result := _size;
end;

destructor TTreeMap<K, V>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TTreeMap<K, V>.GetItem(Key: K): V;
var
  Value: TValue;
  temp: TNode;
begin
  temp := __getNode(_root, Key);
  TValue.Make(@Key, TypeInfo(K), Value);

  if temp = nil then
    raise Exception.Create('There is no ''' + Value.ToString + '''');

  Result := temp.Value;
end;

function TTreeMap<K, V>.IsEmpty: boolean;
begin
  Result := _size = 0;
end;

function TTreeMap<K, V>.Keys: TImpl_K.TArr;
var
  list: TList_node;
  res: TImpl_K.TArr;
  i: integer;
begin
  list := TList_node.Create;
  try
    __inOrder(_root, list);
    SetLength(res, list.Count);

    for i := 0 to list.Count - 1 do
    begin
      res[i] := list[i].Key;
    end;

    Result := res;
  finally
    list.Free;
  end;
end;

function TTreeMap<K, V>.Remove(Key: K): TPtr_V;
var
  cur: TNode;
  res: TPtr_V;
begin
  res := nil;
  cur := __getNode(_root, Key);

  if cur <> nil then
  begin
    res := TPtr_V.Create(cur.Value);
    __remove(cur);
  end;

  Result := res;
end;

procedure TTreeMap<K, V>.SetItem(Key: K; Value: V);
var
  val: TValue;
  temp: TNode;
begin
  temp := __getNode(_root, Key);
  TValue.Make(@Key, TypeInfo(K), val);

  if temp = nil then
    raise Exception.Create('There is no ''' + val.ToString + '''');

  temp.Value := Value;
end;

function TTreeMap<K, V>.Values: TImpl_V.TArr;
var
  list: TList_node;
  res: TImpl_V.TArr;
  i: integer;
begin
  list := TList_node.Create;
  try
    __inOrder(_root, list);

    SetLength(res, list.Count);

    for i := 0 to list.Count - 1 do
    begin
      res[i] := list[i].Value;
    end;

    Result := res;
  finally
    list.Free;
  end;
end;

procedure TTreeMap<K, V>.__afterAdd(node: TNode);
var
  Parent, uncle, grand: TNode;
begin
  Parent := node.Parent;

  // 添加的是根节点 或者 上溢到达了根节点
  if Parent = nil then
  begin
    __setBlack(node);
    Exit;
  end;

  // 如果父节点是黑色，直接返回
  if __isBlack(Parent) then
    Exit;

  // 叔父节点
  uncle := Parent.Sibling;

  // 祖父节点
  grand := __setRed(Parent.Parent);
  if __isRed(uncle) then // 叔父节点是红色【B树节点上溢】
  begin
    __setBlack(Parent);
    __setBlack(uncle);
    // 把祖父节点当做是新添加的节点
    __afterAdd(grand);
    Exit;
  end;

  // 叔父节点不是红色
  if Parent.IsLeftChild then // L
  begin
    if node.IsLeftChild then // LL
    begin
      __setBlack(Parent);
      __rotateRight(grand);
    end
    else // LR
    begin
      __setBlack(node);
      __rotateLeft(Parent);
      __rotateRight(grand);
    end;
  end
  else // R
  begin
    if node.IsLeftChild then // RL
    begin
      __setBlack(node);
      __rotateRight(Parent);
    end
    else // RR
    begin
      __setBlack(Parent);
    end;

    __rotateLeft(grand);
  end;
end;

procedure TTreeMap<K, V>.__afterRemove(node: TNode);
var
  Parent, Sibling: TNode;
  isLeft, isParentBlack: boolean;
begin
  // 如果删除的节点是红色
  // 或者 用以取代删除节点的子节点是红色
  if __isRed(node) then
  begin
    __setBlack(node);
    Exit;
  end;

  Parent := node.Parent;

  // 删除的是根节点
  if Parent = nil then
    Exit;

  // 删除的是黑色叶子节点【下溢】
  // 判断被删除的node是左还是右
  isLeft := (Parent.Left = nil) or (node.IsLeftChild);
  if isLeft then
    Sibling := Parent.Right
  else
    Sibling := Parent.Left;

  if isLeft then // 被删除的节点在左边，兄弟节点在右边
  begin
    if __isRed(Sibling) then // 兄弟节点是红色
    begin
      __setBlack(Sibling);
      __setRed(Parent);
      __rotateLeft(Parent);
      // 更换兄弟
      Sibling := Parent.Right;
    end;

    // 兄弟节点必然是黑色
    if __isBlack(Sibling.Left) and __isBlack(Sibling.Right) then
    begin
      // 兄弟节点没有1个红色子节点，父节点要向下跟兄弟节点合并
      isParentBlack := __isBlack(Parent);
      __setBlack(Parent);
      __setRed(Sibling);

      if (isParentBlack) then
        __afterRemove(Parent);
    end
    else // 兄弟节点至少有1个红色子节点，向兄弟节点借元素
    begin
      // 兄弟节点的左边是黑色，兄弟要先旋转
      if __isBlack(Sibling.Right) then
      begin
        __rotateRight(Sibling);
        Sibling := Parent.Right;
      end;

      __setColor(Sibling, __GetColor(Parent));
      __setBlack(Sibling.Right);
      __setBlack(Parent);
      __rotateLeft(Parent);
    end;
  end
  else // 被删除的节点在右边，兄弟节点在左边
  begin
    if __isRed(Sibling) then // 兄弟节点是红色
    begin
      __setBlack(Sibling);
      __setRed(Parent);
      __rotateRight(Parent);
      // 更换兄弟
      Sibling := Parent.Left;
    end;

    // 兄弟节点必然是黑色
    if __isBlack(Sibling.Left) and __isBlack(Sibling.Right) then
    begin
      // 兄弟节点没有1个红色子节点，父节点要向下跟兄弟节点合并
      isParentBlack := __isBlack(Parent);
      __setBlack(Parent);
      __setRed(Sibling);

      if isParentBlack then
        __afterRemove(Parent);
    end
    else // 兄弟节点至少有1个红色子节点，向兄弟节点借元素
    begin
      // 兄弟节点的左边是黑色，兄弟要先旋转
      if __isBlack(Sibling.Left) then
      begin
        __rotateLeft(Sibling);
        Sibling := Parent.Left;
      end;

      __setColor(Sibling, __GetColor(Parent));
      __setBlack(Sibling.Left);
      __setBlack(Parent);
      __rotateRight(Parent);
    end;
  end;
end;

procedure TTreeMap<K, V>.__afterRotate(grand, Parent, child: TNode);
begin
  // 让parent成为子树的根节点
  Parent.Parent := grand.Parent;

  if grand.IsLeftChild then
  begin
    grand.Parent.Left := Parent;
  end
  else if grand.IsRightChild then
  begin
    grand.Parent.Right := Parent;
  end
  else // grand是 root节点
  begin
    _root := Parent;
  end;

  // 更新 child 的 parent
  if child <> nil then
  begin
    child.Parent := grand;
  end;

  // 更新 grand 的 parent
  grand.Parent := Parent;
end;

procedure TTreeMap<K, V>.__clear(node: TNode);
begin
  if node = nil then
    Exit;

  __clear(node.Left);
  __clear(node.Right);

  if node.Parent <> nil then
  begin
    if node.IsLeftChild then
      node.Parent.Left := nil
    else
      node.Parent.Right := nil;
  end;

  FreeAndNil(node);
end;

function TTreeMap<K, V>.__CreateNode(newKey: K; newValue: V; newParent: TNode): TNode;
begin
  Result := TNode.Create(newKey, newValue, newParent);
end;

function TTreeMap<K, V>.__GetColor(node: TNode): boolean;
begin
  if node = nil then
  begin
    Result := BLACK;
    Exit;
  end;

  Result := node.Color;
end;

function TTreeMap<K, V>.__getNode(node: TNode; Key: K): TNode;
var
  cmp: integer;
begin
  if node = nil then
    Exit(nil);

  cmp := _cmp_K.Compare(Key, node.Key);
  if cmp < 0 then
  begin
    Result := __getNode(node.Left, Key);
  end
  else if cmp > 0 then
  begin
    Result := __getNode(node.Right, Key);
  end
  else
  begin
    Result := node;
  end;
end;

function TTreeMap<K, V>.__getSuccessor(Key: K): TNode;
var
  cur, Parent: TNode;
begin
  cur := __getNode(_root, Key);

  if cur = nil then
  begin
    Result := nil;
    Exit;
  end;

  if cur.Right <> nil then
  begin
    Result := __minNode(cur.Right);
    Exit;
  end;

  Parent := cur.Parent;
  while (Parent <> nil) and (Parent.Right = cur) do
  begin
    cur := cur.Parent;
    Parent := cur.Parent;
  end;

  Result := Parent;
end;

procedure TTreeMap<K, V>.__inOrder(node: TNode; list: TList_node);
begin
  if node = nil then
    Exit;

  __inOrder(node.Left, list);
  list.AddLast(node);
  __inOrder(node.Right, list);
end;

function TTreeMap<K, V>.__isBlack(node: TNode): boolean;
begin
  Result := __GetColor(node) = BLACK;
end;

function TTreeMap<K, V>.__isRed(node: TNode): boolean;
begin
  Result := __GetColor(node) = RED;
end;

function TTreeMap<K, V>.__minNode(node: TNode): TNode;
var
  cur: TNode;
begin
  if node = nil then
    Exit(nil);

  cur := node;

  while cur.Left <> nil do
  begin
    cur := cur.Left;
  end;

  Result := cur;
end;

procedure TTreeMap<K, V>.__remove(node: TNode);
var
  min, replace: TNode;
begin
  if node = nil then
    Exit;

  if node.HasTwoChildren then // 度为2的节点
  begin
    // 找到后继节点
    min := __getSuccessor(node.Key);

    // 用后继节点的值覆盖度为2的节点的值
    node.Key := min.Key;
    node.Value := min.Value;

    // 删除后继节点
    node := min;
  end;

  if node.Left <> nil then
    replace := node.Left
  else
    replace := node.Right;

  if replace <> nil then // node是度为1的节点
  begin
    // 更改parent
    replace.Parent := node.Parent;

    // 更改parent的left、right的指向
    if node.Parent = nil then // node是度为1的节点并且是根节点
    begin
      _root := replace;
    end
    else if node.IsLeftChild then
    begin
      node.Parent.Left := replace;
    end
    else // node = node.parent.right
    begin
      node.Parent.Right := replace;
    end;

    // 删除节点后的调整
    __afterRemove(replace);
  end
  else if node.Parent = nil then // node是叶子节点并且是根节点
  begin
    _root := nil;

    // 删除节点后的调整
    __afterRemove(node);
  end
  else // node是叶子节点，但不是根节点
  begin
    if node.IsLeftChild then
    begin
      node.Parent.Left := nil;
    end
    else // node = node.parent.right
    begin
      node.Parent.Right := nil;
    end;

    // 删除节点后的调整
    __afterRemove(node);
  end;

  _size := _size - 1;
  FreeAndNil(node);
end;

procedure TTreeMap<K, V>.__rotateLeft(grand: TNode);
var
  Parent, child: TNode;
begin
  Parent := grand.Right;
  child := Parent.Left;
  grand.Right := child;
  Parent.Left := grand;

  __afterRotate(grand, Parent, child);
end;

procedure TTreeMap<K, V>.__rotateRight(grand: TNode);
var
  Parent, child: TNode;
begin
  Parent := grand.Left;
  child := Parent.Right;
  grand.Left := child;
  Parent.Right := grand;

  __afterRotate(grand, Parent, child);
end;

function TTreeMap<K, V>.__setBlack(node: TNode): TNode;
begin
  Result := __setColor(node, BLACK);
end;

function TTreeMap<K, V>.__setColor(node: TNode; Color: boolean): TNode;
begin
  if node = nil then
  begin
    Result := node;
    Exit;
  end;

  node.Color := Color;
  Result := node;
end;

function TTreeMap<K, V>.__setRed(node: TNode): TNode;
begin
  Result := __setColor(node, RED);
end;

{ TTreeMap<K, V>.TNode }

constructor TTreeMap<K, V>.TNode.Create(newKey: K; newValue: V; newParent: TNode);
begin
  Key := newKey;
  Value := newValue;
  Parent := newParent;
  Color := RED;
end;

function TTreeMap<K, V>.TNode.HasTwoChildren: boolean;
begin
  Result := (Left <> nil) and (Right <> nil);
end;

function TTreeMap<K, V>.TNode.IsLeaf: boolean;
begin
  Result := (Left = nil) and (Right = nil);
end;

function TTreeMap<K, V>.TNode.IsLeftChild: boolean;
begin
  Result := (Parent <> nil) and (Parent.Left = Self);
end;

function TTreeMap<K, V>.TNode.IsRightChild: boolean;
begin
  Result := (Parent <> nil) and (Parent.Right = Self);
end;

function TTreeMap<K, V>.TNode.Sibling: TNode;
var
  res: TNode;
begin
  if IsLeftChild then
    res := Parent.Right
  else if IsRightChild then
    res := Parent.Left
  else
    res := nil;

  Result := res;
end;

end.
