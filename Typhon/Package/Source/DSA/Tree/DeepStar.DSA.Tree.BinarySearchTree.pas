unit DeepStar.DSA.Tree.BinarySearchTree;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.DSA.Tree.BinaryTree;

type
  generic TBinarySearchTree<K, V> = class(specialize TBinaryTree<K, V>)
  protected
    // 增加节点后的调整
    procedure __afterAdd(node: TNode); virtual;
    // 删除节点后的调整
    procedure __afterRemove(node: TNode); virtual;
    procedure __remove(node: TNode);

  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(key: K; Value: V); override;
    procedure Remove(key: K); override;
  end;

implementation

{ TBinarySearchTree }

constructor TBinarySearchTree.Create;
begin
  inherited Create;
end;

procedure TBinarySearchTree.Add(key: K; Value: V);
var
  parent, cur: TNode;
  cmp: integer;
begin
  parent := nil;
  cur := _root;
  cmp := 0;

  while cur <> nil do
  begin
    parent := cur;
    cmp := _cmp_K.Compare(key, cur.Key);

    if cmp < 0 then
      cur := cur.Left
    else if cmp > 0 then
      cur := cur.Right
    else
      Exit;
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
end;

destructor TBinarySearchTree.Destroy;
begin
  inherited Destroy;
end;

procedure TBinarySearchTree.Remove(key: K);
begin
  __remove(__getNode(_root, key));
end;

procedure TBinarySearchTree.__afterAdd(node: TNode);
begin
  Exit;
end;

procedure TBinarySearchTree.__afterRemove(node: TNode);
begin
  Exit;
end;

procedure TBinarySearchTree.__remove(node: TNode);
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

end.
