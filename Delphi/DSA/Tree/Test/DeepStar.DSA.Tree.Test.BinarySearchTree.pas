unit DeepStar.DSA.Tree.Test.BinarySearchTree;

interface

uses
 System. Classes,
 System. SysUtils,
  DeepStar.DSA.Tree.BinarySearchTree,
  LQA.Utils;

procedure Main;

implementation

type
  TBSTTree_int_int = TBinarySearchTree<integer, integer>;

procedure Main;
var
  tree: TBSTTree_int_int;
  arr: TArr_int;
  i: integer;
begin
  tree := TBSTTree_int_int.Create;

  tree.Add(1, 1);
  tree.Add(2, 2);
  tree.Add(3, 3);

  //tree.Remove(1);

  TArrayUtils_int.Print(tree.Keys);
  TArrayUtils_int.Print(tree.Values);
  writeln('ContainKey(3): ', tree.ContainsKey(3));
  writeln('ContainKey(4): ', tree.ContainsKey(4));
  writeln('ContainsValue(3): ', tree.ContainsValue(3));
  writeln('ContainsValue(4): ', tree.ContainsValue(4));
  writeln('tree.GetItem(2): ', tree.GetItem(2));
  writeln('tree.GetHeight: ', tree.GetHeight);

  tree.Clear;

  DrawLineBlockEnd;

  arr := [4, 2, 3, 1, 8, 6, 7, 5, 9, 10];

  for i := 0 to High(arr) do
  begin
    tree.Add(arr[i], i);
  end;

  TArrayUtils_int.Print(tree.Keys);

  DrawLineBlockEnd;

  tree.Remove(1);
  TArrayUtils_int.Print(tree.Keys);
  writeln('tree.IsComplete: ', tree.IsComplete);
end;

end.
