unit DeepStar.DSA.Tree.BinaryTree;

interface

uses
  System.SysUtils,
  System.Math,
  System.Rtti,
  DeepStar.DSA.Interfaces,
  DeepStar.DSA.Linear.ArrayList,
  DeepStar.DSA.Linear.Queue;

type
  TBinaryTree<K, V> = class abstract
  public type
    TNode = class(TObject)
    public
      Key: K;
      Value: V;
      Left: TNode;
      Right: TNode;
      Parent: TNode;

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

    TImpl_K = TImpl<K>;
    TImpl_V = TImpl<V>;
    TList_node = TArrayList<TNode>;
    TQueue_node = TQueue<TNode>;

  protected
    _root: TNode;
    _cmp_K: TImpl_K.ICmp;
    _cmp_V: TImpl_V.ICmp;
    _size: integer;

    function __CreateNode(newKey: K; newValue: V; newParent: TNode): TNode; virtual;
    function __getHeight(node: TNode): integer;
    function __getNode(key: K): TNode; overload;
    function __getNode(node: TNode; key: K): TNode; overload;
    function __getPredecessor(key: K): TNode;
    function __getSuccessor(key: K): TNode;
    function __maxNode(node: TNode): TNode;
    function __minNode(node: TNode): TNode;
    procedure __clear(node: TNode);
    procedure __inOrder(node: TNode; list: TList_node);

  public
    constructor Create;
    destructor Destroy; override;

    function ContainsKey(key: K): boolean;
    function ContainsValue(Value: V): boolean;
    function Count: integer;
    function GetItem(key: K): V;
    function GetHeight: integer;
    function IsComplete: boolean;
    function IsEmpty: boolean;
    function Keys: TImpl_K.TArr;
    function Values: TImpl_V.TArr;
    procedure Add(key: K; Value: V); virtual; abstract;
    procedure Clear;
    procedure Remove(key: K); virtual; abstract;
    procedure SetItem(key: K; newValue: V);

    property Comparer_K: TImpl_K.ICmp read _cmp_K write _cmp_K;
    property Comparer_V: TImpl_V.ICmp read _cmp_V write _cmp_V;
    property Item[key: K]: V read GetItem write SetItem; default;
  end;

implementation

{ TBinaryTree.TNode }

constructor TBinaryTree<K, V>.TNode.Create(newKey: K; newValue: V; newParent: TNode);
begin
  Key := newKey;
  Value := newValue;
  Parent := newParent;
end;

function TBinaryTree<K, V>.TNode.HasTwoChildren: boolean;
begin
  Result := (Left <> nil) and (Right <> nil);
end;

function TBinaryTree<K, V>.TNode.IsLeaf: boolean;
begin
  Result := (Left = nil) and (Right = nil);
end;

function TBinaryTree<K, V>.TNode.IsLeftChild: boolean;
begin
  Result := (Parent <> nil) and (Parent.Left = Self);
end;

function TBinaryTree<K, V>.TNode.IsRightChild: boolean;
begin
  Result := (Parent <> nil) and (Parent.Right = Self);
end;

function TBinaryTree<K, V>.TNode.Sibling: TNode;
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

{ TBinaryTree<K,V> }

constructor TBinaryTree<K, V>.Create;
begin
  _root := nil;
  _size := 0;
  _cmp_K := TImpl_K.TCmp.Default;
  _cmp_V := TImpl_V.TCmp.Default;
end;

procedure TBinaryTree<K, V>.Clear;
begin
  __clear(_root);
  _root := nil;
  _size := 0;
end;

function TBinaryTree<K, V>.ContainsKey(key: K): boolean;
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

function TBinaryTree<K, V>.ContainsValue(Value: V): boolean;
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

      if _cmp_V.Compare(value, cur.Value) = 0 then
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

function TBinaryTree<K, V>.Count: integer;
begin
  Result := _size;
end;

destructor TBinaryTree<K, V>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TBinaryTree<K, V>.GetItem(key: K): V;
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

function TBinaryTree<K, V>.IsComplete: boolean;
var
  queue: TQueue_node;
  leaf: boolean;
  node: TNode;
begin
  if _root = nil then
  begin
    Exit(false);
  end;

  queue := TQueue_node.Create;
  queue.EnQueue(_root);

  leaf := false;
  while not queue.IsEmpty do
  begin
    node := queue.DeQueue;

    if leaf and not(node.IsLeaf) then // 要求是叶子结点，但是当前节点不是叶子结点
    begin
      Exit(false);
    end;

    if node.left <> nil then
    begin
      queue.EnQueue(node.left);
    end
    else if node.right <> nil then
    begin
      Exit(false);
    end;

    if node.right <> nil then
    begin
      queue.EnQueue(node.right);
    end
    else
    begin
      leaf := true; // 要求后面都是叶子节点
    end;
  end;

  Result := true;
end;

function TBinaryTree<K, V>.GetHeight: integer;
begin
  Result := __getHeight(_root);
end;

function TBinaryTree<K, V>.IsEmpty: boolean;
begin
  Result := _size = 0;
end;

function TBinaryTree<K, V>.Keys: TImpl_K.TArr;
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

procedure TBinaryTree<K, V>.SetItem(key: K; newValue: V);
var
  Value: TValue;
  temp: TNode;
begin
  temp := __getNode(_root, key);
  TValue.Make(@key, TypeInfo(K), Value);

  if temp = nil then
    raise Exception.Create('There is no ''' + Value.ToString + '''');

  temp.Value := newValue;
end;

function TBinaryTree<K, V>.Values: TImpl_V.TArr;
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

procedure TBinaryTree<K, V>.__clear(node: TNode);
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

function TBinaryTree<K, V>.__CreateNode(newKey: K; newValue: V; newParent: TNode): TNode;
begin
  Result := TNode.Create(newKey, newValue, newParent);
end;

function TBinaryTree<K, V>.__getPredecessor(key: K): TNode;
var
  cur, parent: TNode;
begin
  cur := __getNode(_root, key);

  if cur = nil then
  begin
    Result := nil;
    Exit;
  end;

  if cur.Left <> nil then
  begin
    Result := __maxNode(cur.Left);
    Exit;
  end;

  parent := cur.Parent;
  while (parent <> nil) and (parent.Left = cur) do
  begin
    cur := cur.Parent;
    parent := cur.Parent;
  end;

  Result := parent;
end;

function TBinaryTree<K, V>.__getSuccessor(key: K): TNode;
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

procedure TBinaryTree<K, V>.__inOrder(node: TNode; list: TList_node);
begin
  if node = nil then
    Exit;

  __inOrder(node.Left, list);
  list.AddLast(node);
  __inOrder(node.Right, list);
end;

function TBinaryTree<K, V>.__getHeight(node: TNode): integer;
begin
  if node = nil then
    Exit(0);

  Result := 1 + Max(__getHeight(node.Left), __getHeight(node.Right));
end;

function TBinaryTree<K, V>.__getNode(key: K): TNode;
begin
  Result := __getNode(_root, key);
end;

function TBinaryTree<K, V>.__getNode(node: TNode; key: K): TNode;
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

function TBinaryTree<K, V>.__maxNode(node: TNode): TNode;
var
  cur: TNode;
begin
  if node = nil then
    Exit(nil);

  cur := node;

  while cur.Right <> nil do
  begin
    cur := cur.Right;
  end;

  Result := cur;
end;

function TBinaryTree<K, V>.__minNode(node: TNode): TNode;
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

end.
