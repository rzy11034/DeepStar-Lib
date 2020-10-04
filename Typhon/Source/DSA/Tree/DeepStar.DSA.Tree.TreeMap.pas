unit DeepStar.DSA.Tree.TreeMap;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  {%H-}Rtti,
  DeepStar.DSA.Interfaces,
  DeepStar.DSA.Linear.Queue,
  DeepStar.DSA.Linear.ArrayList;

type
  // 用红黑树实现
  generic TTreeMap<K, V> = class(TInterfacedObject, specialize IMap<K, V>)
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

    TPtrValue = specialize TPtrValue<V>;
    TImpl_K = specialize TImpl<K>;
    TImpl_V = specialize TImpl<V>;
    TList_node = specialize TArrayList<TNode>;
    TQueue_node = specialize TQueue<TNode>;

  private
    _cmp_K: TImpl_K.ICmp;
    _cmp_V: TImpl_V.ICmp;
    _root: TNode;
    _size: integer;

    function __CreateNode(newKey: K; newValue: V; newParent: TNode): TNode;
    function __GetColor(node: TNode): boolean;
    function __getNode(node: TNode; key: K): TNode;
    function __getSuccessor(key: K): TNode;
    function __isBlack(node: TNode): boolean;
    function __isRed(node: TNode): boolean;
    function __minNode(node: TNode): TNode;
    function __setBlack(node: TNode): TNode;
    function __setColor(node: TNode; color: boolean): TNode;
    function __setRed(node: TNode): TNode;
    procedure __afterAdd(node: TNode);
    procedure __afterRemove(node: TNode);
    procedure __afterRotate(grand, parent, child: TNode);
    procedure __clear(node: TNode);
    procedure __inOrder(node: TNode; list: TList_node);
    procedure __remove(node: TNode);
    procedure __rotateLeft(grand: TNode);
    procedure __rotateRight(grand: TNode);

  public
    constructor Create;
    destructor Destroy; override;

    function Add(key: K; Value: V): TPtrValue;
    function ContainsKey(key: K): boolean;
    function ContainsValue(Value: V): boolean;
    function Count: integer;
    function GetItem(key: K): V;
    function IsEmpty: boolean;
    function Keys: TImpl_K.TArr;
    function Remove(key: K): TPtrValue;
    function Values: TImpl_V.TArr;
    procedure Clear;
    procedure SetItem(key: K; Value: V);

    property Comparer_K: TImpl_K.ICmp read _cmp_K write _cmp_K;
    property Comparer_V: TImpl_V.ICmp read _cmp_V write _cmp_V;
    property Item[key: K]: V read GetItem write SetItem; default;
  end;

implementation

{ TTreeMap }

constructor TTreeMap.Create;
begin
  _root := nil;
  _size := 0;
  _cmp_K := TImpl_K.TCmp.Default;
  _cmp_V := TImpl_V.TCmp.Default;
end;

function TTreeMap.Add(key: K; Value: V): TPtrValue;
var
  parent, cur: TNode;
  cmp: integer;
  res: TPtrValue;
begin
  parent := nil;
  cur := _root;
  cmp := 0;
  res := nil;

  while cur <> nil do
  begin
    parent := cur;
    cmp := _cmp_K.Compare(key, cur.Key);

    if cmp < 0 then
      cur := cur.Left
    else if cmp > 0 then
      cur := cur.Right
    else
    begin
      res := TPtrValue.Create(cur.Value);
      cur.Value := Value;
      Exit;
    end;
  end;

  cur := __CreateNode(key, Value, parent);
  if parent = nil then
  begin
    _root := cur;
    __afterAdd(cur);
  end
  else if cmp < 0 then
  begin
    parent.Left := cur;
    __afterAdd(cur);
  end
  else if cmp > 0 then
  begin
    parent.Right := cur;
    __afterAdd(cur);
  end;

  _size += 1;
  Result := res;
end;

procedure TTreeMap.Clear;
begin
  __clear(_root);
  _root := nil;
  _size := 0;
end;

function TTreeMap.ContainsKey(key: K): boolean;
var
  cur: TNode;
  cmp: integer;
begin
  cur := _root;

  while cur <> nil do
  begin
    cmp := _cmp_K.Compare(key, cur.Key);
    if cmp < 0 then
      cur := cur.Left
    else if cmp > 0 then
      cur := cur.Right
    else
      Exit(true);
  end;

  Result := false;
end;

function TTreeMap.ContainsValue(Value: V): boolean;
var
  queue: TQueue_node;
  cur: TNode;
begin
  if _root = nil then
    Exit(false);

  cur := _root;

  queue := TQueue_node.Create;
  try
    queue.EnQueue(cur);

    while not queue.IsEmpty do
    begin
      cur := queue.DeQueue;

      if _cmp_V.Compare(Value, cur.Value) = 0 then
      begin
        Result := true;
        Exit;
      end;

      if cur.Left <> nil then
        queue.EnQueue(cur.Left);
      if cur.Right <> nil then
        queue.EnQueue(cur.Right);
    end;

    Result := false;
  finally
    queue.Free;
  end;
end;

function TTreeMap.Count: integer;
begin
  Result := _size;
end;

destructor TTreeMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TTreeMap.GetItem(key: K): V;
var
  Value: TValue;
  temp: TNode;
begin
  temp := __getNode(_root, key);
  TValue.Make(@key, TypeInfo(K), Value);

  if temp = nil then
    raise Exception.Create('There is no ''' + Value.ToString + '''');

  Result := temp.Value;
end;

function TTreeMap.IsEmpty: boolean;
begin
  Result := _size = 0;
end;

function TTreeMap.Keys: TImpl_K.TArr;
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

function TTreeMap.Remove(key: K): TPtrValue;
var
  cur: TNode;
  res: TPtrValue;
begin
  res := nil;
  cur := __getNode(_root, key);

  if cur <> nil then
  begin
    res := TPtrValue.Create(cur.Value);
    __remove(cur);
  end;

  Result := res;
end;

procedure TTreeMap.SetItem(key: K; Value: V);
var
  val: TValue;
  temp: TNode;
begin
  temp := __getNode(_root, key);
  TValue.Make(@key, TypeInfo(K), Val);

  if temp = nil then
    raise Exception.Create('There is no ''' + val.ToString + '''');

  temp.Value := Value;
end;

function TTreeMap.Values: TImpl_V.TArr;
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

procedure TTreeMap.__afterAdd(node: TNode);
var
  parent, uncle, grand: TNode;
begin
  parent := node.Parent;

  // 添加的是根节点 或者 上溢到达了根节点
  if parent = nil then
  begin
    __setBlack(node);
    Exit;
  end;

  // 如果父节点是黑色，直接返回
  if __isBlack(parent) then
    Exit;

  // 叔父节点
  uncle := parent.Sibling;

  // 祖父节点
  grand := __setRed(parent.parent);
  if __isRed(uncle) then // 叔父节点是红色【B树节点上溢】
  begin
    __setBlack(parent);
    __setBlack(uncle);
    // 把祖父节点当做是新添加的节点
    __afterAdd(grand);
    Exit;
  end;

  // 叔父节点不是红色
  if parent.IsLeftChild then  // L
  begin
    if node.IsLeftChild then // LL
    begin
      __setBlack(parent);
      __rotateRight(grand);
    end
    else // LR
    begin
      __setBlack(node);
      __rotateLeft(parent);
      __rotateRight(grand);
    end;
  end
  else // R
  begin
    if node.IsLeftChild then // RL
    begin
      __setBlack(node);
      __rotateRight(parent);
    end
    else // RR
    begin
      __setBlack(parent);
    end;

    __rotateLeft(grand);
  end;
end;

procedure TTreeMap.__afterRemove(node: TNode);
var
  parent, sibling: TNode;
  isLeft, isParentBlack: boolean;
begin
  // 如果删除的节点是红色
  // 或者 用以取代删除节点的子节点是红色
  if __isRed(node) then
  begin
    __setBlack(node);
    Exit;
  end;

  parent := node.parent;

  // 删除的是根节点
  if parent = nil then
    Exit;

  // 删除的是黑色叶子节点【下溢】
  // 判断被删除的node是左还是右
  isLeft := (parent.Left = nil) or (node.isLeftChild);
  if isLeft then
    sibling := parent.Right
  else
    sibling := parent.Left;

  if isLeft then // 被删除的节点在左边，兄弟节点在右边
  begin
    if __isRed(sibling) then // 兄弟节点是红色
    begin
      __setBlack(sibling);
      __setRed(parent);
      __rotateLeft(parent);
      // 更换兄弟
      sibling := parent.right;
    end;

    // 兄弟节点必然是黑色
    if __isBlack(sibling.Left) and __isBlack(sibling.right) then
    begin
      // 兄弟节点没有1个红色子节点，父节点要向下跟兄弟节点合并
      isParentBlack := __isBlack(parent);
      __setBlack(parent);
      __setRed(sibling);

      if (isParentBlack) then
        __afterRemove(parent);
    end
    else // 兄弟节点至少有1个红色子节点，向兄弟节点借元素
    begin
      // 兄弟节点的左边是黑色，兄弟要先旋转
      if __isBlack(sibling.right) then
      begin
        __rotateRight(sibling);
        sibling := parent.Right;
      end;

      __setColor(sibling, __GetColor(parent));
      __setBlack(sibling.Right);
      __setBlack(parent);
      __rotateLeft(parent);
    end;
  end
  else // 被删除的节点在右边，兄弟节点在左边
  begin
    if __isRed(sibling) then // 兄弟节点是红色
    begin
      __setBlack(sibling);
      __setRed(parent);
      __rotateRight(parent);
      // 更换兄弟
      sibling := parent.Left;
    end;

    // 兄弟节点必然是黑色
    if __isBlack(sibling.Left) and __isBlack(sibling.right) then
    begin
      // 兄弟节点没有1个红色子节点，父节点要向下跟兄弟节点合并
      isParentBlack := __isBlack(parent);
      __setBlack(parent);
      __setRed(sibling);

      if isParentBlack then
        __afterRemove(parent);
    end
    else  // 兄弟节点至少有1个红色子节点，向兄弟节点借元素
    begin
      // 兄弟节点的左边是黑色，兄弟要先旋转
      if __isBlack(sibling.Left) then
      begin
        __rotateLeft(sibling);
        sibling := parent.Left;
      end;

      __setColor(sibling, __GetColor(parent));
      __setBlack(sibling.Left);
      __setBlack(parent);
      __rotateRight(parent);
    end;
  end;
end;

procedure TTreeMap.__afterRotate(grand, parent, child: TNode);
begin
  // 让parent成为子树的根节点
  parent.parent := grand.parent;

  if grand.IsLeftChild then
  begin
    grand.parent.left := parent;
  end
  else if grand.IsRightChild then
  begin
    grand.parent.right := parent;
  end
  else // grand是 root节点
  begin
    _root := parent;
  end;

  // 更新 child 的 parent
  if child <> nil then
  begin
    child.parent := grand;
  end;

  // 更新 grand 的 parent
  grand.parent := parent;
end;

procedure TTreeMap.__clear(node: TNode);
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

function TTreeMap.__CreateNode(newKey: K; newValue: V; newParent: TNode): TNode;
begin
  Result := TNode.Create(newKey, newValue, newParent);
end;

function TTreeMap.__GetColor(node: TNode): boolean;
begin
  if node = nil then
  begin
    Result := BLACK;
    Exit;
  end;

  Result := node.Color;
end;

function TTreeMap.__getNode(node: TNode; key: K): TNode;
var
  cmp: integer;
begin
  if node = nil then
    Exit(nil);

  cmp := _cmp_K.Compare(key, node.Key);
  if cmp < 0 then
  begin
    Result := __getNode(node.Left, key);
  end
  else if cmp > 0 then
  begin
    Result := __getNode(node.Right, key);
  end
  else
  begin
    Result := node;
  end;
end;

function TTreeMap.__getSuccessor(key: K): TNode;
var
  cur, parent: TNode;
begin
  cur := __getNode(_root, key);

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

  parent := cur.Parent;
  while (Parent <> nil) and (parent.Right = cur) do
  begin
    cur := cur.Parent;
    parent := cur.Parent;
  end;

  Result := parent;
end;

procedure TTreeMap.__inOrder(node: TNode; list: TList_node);
begin
  if node = nil then
    Exit;

  __inOrder(node.Left, list);
  list.AddLast(node);
  __inOrder(node.Right, list);
end;

function TTreeMap.__isBlack(node: TNode): boolean;
begin
  Result := __GetColor(node) = BLACK;
end;

function TTreeMap.__isRed(node: TNode): boolean;
begin
  Result := __GetColor(node) = RED;
end;

function TTreeMap.__minNode(node: TNode): TNode;
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

procedure TTreeMap.__remove(node: TNode);
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
    replace.parent := node.parent;

    // 更改parent的left、right的指向
    if node.parent = nil then // node是度为1的节点并且是根节点
    begin
      _root := replace;
    end
    else if node.IsLeftChild then
    begin
      node.parent.left := replace;
    end
    else // node = node.parent.right
    begin
      node.parent.right := replace;
    end;

    // 删除节点后的调整
    __afterRemove(replace);
  end
  else if node.parent = nil then // node是叶子节点并且是根节点
  begin
    _root := nil;

    // 删除节点后的调整
    __afterRemove(node);
  end
  else // node是叶子节点，但不是根节点
  begin
    if node.IsLeftChild then
    begin
      node.parent.left := nil;
    end
    else // node = node.parent.right
    begin
      node.parent.right := nil;
    end;

    // 删除节点后的调整
    __afterRemove(node);
  end;

  _size -= 1;
  FreeAndNil(node);
end;

procedure TTreeMap.__rotateLeft(grand: TNode);
var
  parent, child: TNode;
begin
  parent := grand.right;
  child := parent.left;
  grand.right := child;
  parent.left := grand;

  __afterRotate(grand, parent, child);
end;

procedure TTreeMap.__rotateRight(grand: TNode);
var
  parent, child: TNode;
begin
  parent := grand.left;
  child := parent.right;
  grand.left := child;
  parent.right := grand;

  __afterRotate(grand, parent, child);
end;

function TTreeMap.__setBlack(node: TNode): TNode;
begin
  Result := __setColor(node, BLACK);
end;

function TTreeMap.__setColor(node: TNode; color: boolean): TNode;
begin
  if node = nil then
  begin
    Result := node;
    Exit;
  end;

  node.Color := color;
  Result := node;
end;

function TTreeMap.__setRed(node: TNode): TNode;
begin
  Result := __setColor(node, RED);
end;

{ TTreeMap.TNode }

constructor TTreemap.TNode.Create(newKey: K; newValue: V; newParent: TNode);
begin
  Key := newKey;
  Value := newValue;
  Parent := newParent;
  Color := RED;
end;

function TTreemap.TNode.HasTwoChildren: boolean;
begin
  Result := (Left <> nil) and (Right <> nil);
end;

function TTreemap.TNode.IsLeaf: boolean;
begin
  Result := (Left = nil) and (Right = nil);
end;

function TTreemap.TNode.IsLeftChild: boolean;
begin
  Result := (Parent <> nil) and (Parent.Left = Self);
end;

function TTreemap.TNode.IsRightChild: boolean;
begin
  Result := (Parent <> nil) and (Parent.Right = Self);
end;

function TTreemap.TNode.Sibling: TNode;
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
