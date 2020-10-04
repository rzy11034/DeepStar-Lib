unit DeepStar.DSA.Tree.BalanceBinarySearchTree;

interface

uses
  System.SysUtils,
  DeepStar.DSA.Tree.BinarySearchTree;

type
  TBalanceBinarySearchTree<K, V> = class (TBinarySearchTree<K, V>)
  private type
    TNode = TBinarySearchTree<K, V>.TNode;

  protected
    procedure __rotateLeft(grand: TNode); virtual;
    procedure __rotateRight(grand: TNode); virtual;
    procedure __afterRotate(grand, parent, child: TNode); virtual;
  end;

implementation

{ TBalanceBinarySearchTree }

procedure TBalanceBinarySearchTree<K, V>.__afterRotate(grand, parent, child: TNode);
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

procedure TBalanceBinarySearchTree<K, V>.__rotateLeft(grand: TNode);
var
  parent, child: TNode;
begin
  parent := grand.right;
  child := parent.left;
  grand.right := child;
  parent.left := grand;

  __afterRotate(grand, parent, child);
end;

procedure TBalanceBinarySearchTree<K, V>.__rotateRight(grand: TNode);
var
  parent, child: TNode;
begin
  parent := grand.left;
  child := parent.right;
  grand.left := child;
  parent.right := grand;

  __afterRotate(grand, parent, child);
end;

end.
