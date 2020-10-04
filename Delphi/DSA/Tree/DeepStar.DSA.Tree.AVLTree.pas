unit DeepStar.DSA.Tree.AVLTree;

interface

uses
  System.SysUtils,
  System.Math,
  DeepStar.DSA.Tree.BalanceBinarySearchTree;

type
  TAVLTree<K, V> = class(TBalanceBinarySearchTree<K, V>)
  private type
    TNode = TBalanceBinarySearchTree<K, V>.TNode;

    TAVLNode = class(TBalanceBinarySearchTree<K,V>.TNode)
    public
      Height: integer;

      constructor Create(newKey: K; newValue: V; newParent: TAVLNode);
      // 获取该节点平衡因子
      function BalanceFactor: integer;
      // 比较高的子节点
      function TallerChild: TAVLNode;
      // 更新高度
      procedure UpdateHeight;
      procedure __getAllChildHeight(out leftHeight, rightHeight: integer);
    end;

  protected
    function __CreateNode(newKey: K; newValue: V; newParent: TNode): TNode; override;
    // 判断传入节点是否平衡（平衡因子的绝对值 <= 1）
    function __isBalanced(node: TAVLNode): boolean;
    procedure __afterAdd(node: TNode); override;
    procedure __afterRemove(node: TNode); override;
    // 恢复平衡:  grand--高度最低的那个不平衡节点
    procedure __rebalance(grand: TAVLNode);
    procedure __updateHeight(node: TAVLNode);
    procedure __afterRotate(grand, parent, child: TNode); override;

  public
    constructor Create;
    destructor Destroy; override;
    function Height: integer;
  end;

implementation

{ TAVLTree<K,V> }

constructor TAVLTree<K, V>.Create;
begin
  inherited Create;
end;

destructor TAVLTree<K, V>.Destroy;
begin
  inherited Destroy;
end;

function TAVLTree<K, V>.Height: integer;
begin
  Result := (_root as TAVLNode).Height;
end;

procedure TAVLTree<K, V>.__afterAdd(node: TNode);
begin
  node := node.Parent;

  while node <> nil do
  begin
    if __isBalanced(node as TAVLNode) then // 如果平衡
    begin
      (node as TAVLNode).UpdateHeight;
    end
    else // 如果不平衡, 恢复平衡
    begin
      __rebalance(node as TAVLNode);
      Break;
    end;

    node := node.Parent;
  end;
end;

procedure TAVLTree<K, V>.__afterRemove(node: TNode);
begin
  node := node.Parent;

  while node <> nil do
  begin
    if __isBalanced(node as TAVLNode) then // 如果平衡
    begin
      __updateHeight(node as TAVLNode);
    end
    else // 如果不平衡, 恢复平衡
    begin
      __rebalance(node as TAVLNode);
    end;

    node := node.Parent;
  end;
end;

procedure TAVLTree<K, V>.__afterRotate(grand, parent, child: TNode);
begin
  inherited __afterRotate(grand, parent, child);

  // 更新高度
  __updateHeight(grand as TAVLNode);
  __updateHeight(parent as TAVLNode);
end;

function TAVLTree<K, V>.__CreateNode(newKey: K; newValue: V; newParent: TNode): TNode;
begin
  Result := TAVLNode.Create(newKey, newValue, newParent as TAVLNode);
end;

function TAVLTree<K, V>.__isBalanced(node: TAVLNode): boolean;
begin
  Result := Abs(node.BalanceFactor) <= 1;
end;

procedure TAVLTree<K, V>.__rebalance(grand: TAVLNode);
var
  parent, node: TAVLNode;
begin
  parent := grand.TallerChild;
  node := parent.TallerChild;

  if parent.IsLeftChild then // L
  begin
    if node.IsLeftChild then // LL
    begin
      __rotateRight(grand);
    end
    else // LR
    begin
      __rotateLeft(parent);
      __rotateRight(grand);
    end;
  end
  else // R
  begin
    if node.IsRightChild then // RR
    begin
      __rotateLeft(grand);
    end
    else // RL
    begin
      __rotateRight(parent);
      __rotateLeft(grand);
    end;
  end;
end;

procedure TAVLTree<K, V>.__updateHeight(node: TAVLNode);
begin
  node.UpdateHeight;
end;

{ TAVLTree<K,V>.TAVLNode }

constructor TAVLTree<K, V>.TAVLNode.Create(newKey: K; newValue: V; newParent: TAVLNode);
begin
  inherited Create(newKey, newValue, newParent);
  Height := 1;
end;

function TAVLTree<K, V>.TAVLNode.BalanceFactor: integer;
var
  leftHeight, rightHeight: integer;
begin
  __getAllChildHeight(leftHeight, rightHeight);

  Result := leftHeight - rightHeight;
end;

function TAVLTree<K, V>.TAVLNode.TallerChild: TAVLNode;
var
  leftHeight, rightHeight: integer;
  res: TAVLNode;
begin
  __getAllChildHeight(leftHeight, rightHeight);

  if leftHeight > rightHeight then
    res := Left as TAVLNode
  else if leftHeight < rightHeight then
    res := Right as TAVLNode
  else
  begin
    if IsLeftChild then
      res := left as TAVLNode
    else
      res := Right as TAVLNode;
  end;

  Result := res;
end;

procedure TAVLTree<K, V>.TAVLNode.UpdateHeight;
var
  leftHeight, rightHeight: integer;
begin
  __getAllChildHeight(leftHeight, rightHeight);

  Height := 1 + Max(leftHeight, rightHeight);
end;

procedure TAVLTree<K, V>.TAVLNode.__getAllChildHeight(out leftHeight, rightHeight: integer);
begin
  if Left = nil then
    leftHeight := 0
  else
    leftHeight := (Left as TAVLNode).Height;

  if Right = nil then
    rightHeight := 0
  else
    rightHeight := (Right as TAVLNode).Height;
end;

end.
