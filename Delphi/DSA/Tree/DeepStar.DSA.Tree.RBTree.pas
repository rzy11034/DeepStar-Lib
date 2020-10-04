unit DeepStar.DSA.Tree.RBTree;

interface

uses
System.  SysUtils,
  DeepStar.DSA.Tree.BalanceBinarySearchTree;

type
  TRBTree<K, V> = class(TBalanceBinarySearchTree<K, V>)
  private const
    RED = False;
    BLACK = True;

  private type
    TNode = TBalanceBinarySearchTree<K, V>.TNode;

    TRBNode = class(TNode)
    public
      Color: boolean;

      constructor Create(newKey: K; newValue: V; newParent: TRBNode);
    end;

  private
    // 返回该节点的颜色
    function __GetColor(node: TNode): boolean;
    // 该节点是否为黑色
    function __isBlack(node: TNode): boolean;
    // 该节点是否为红色
    function __isRed(node: TNode): boolean;
    // 染色
    function __setColor(node: TNode; color: boolean): TNode;
    // 将该节点染为红色
    function __setRed(node: TNode): TNode;
    // 将该节点染为黑色
    function __setBlack(node: TNode): TNode;

  protected
    procedure __afterAdd(node: TNode); override;
    procedure __afterRemove(node: TNode); override;
    function __CreateNode(newKey: K; newValue: V; newParent: TNode): TNode; override;

  public
    constructor Create;
    destructor Destroy; override;

  end;

implementation

{ TRBTree<K,V>.TRBNode }

constructor TRBTree<K,V>.TRBNode.Create(newKey: K; newValue: V; newParent: TRBNode);
begin
  inherited Create(newKey, newValue, newParent);
  Color := RED;
end;

{ TRBTree<K,V> }

constructor TRBTree<K,V>.Create;
begin
  inherited Create;
end;

destructor TRBTree<K,V>.Destroy;
begin
  inherited Destroy;
end;

procedure TRBTree<K,V>.__afterAdd(node: TNode);
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

procedure TRBTree<K,V>.__afterRemove(node: TNode);
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

function TRBTree<K,V>.__CreateNode(newKey: K; newValue: V; newParent: TNode): TNode;
begin
  Result := TRBNode.Create(newKey, newValue, newParent as TRBNode);
end;

function TRBTree<K,V>.__GetColor(node: TNode): boolean;
begin
  if node = nil then
  begin
    Result := BLACK;
    Exit;
  end;

  Result := (node as TRBNode).Color;
end;

function TRBTree<K,V>.__isBlack(node: TNode): boolean;
begin
  Result := __GetColor(node) = BLACK;
end;

function TRBTree<K,V>.__isRed(node: TNode): boolean;
begin
  Result := __GetColor(node) = RED;
end;

function TRBTree<K,V>.__setBlack(node: TNode): TNode;
begin
  Result := __setColor(node, BLACK);
end;

function TRBTree<K,V>.__setColor(node: TNode; color: boolean): TNode;
begin
  if node = nil then
  begin
    Result := node;
    Exit;
  end;

  (node as TRBNode).Color := color;
  Result := node;
end;

function TRBTree<K,V>.__setRed(node: TNode): TNode;
begin
  Result := __setColor(node, RED);
end;

end.
