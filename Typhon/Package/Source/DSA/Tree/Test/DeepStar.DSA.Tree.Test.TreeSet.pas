unit DeepStar.DSA.Tree.Test.TreeSet;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.DSA.Tree.TreeSet,
  DeepStar.Utils;

procedure Main;

implementation

type
  TSet_int = specialize TTreeSet<integer>;

procedure Main;
var
  tree: TSet_int;
  arr: TArr_int;
  i: integer;
begin
  arr := [55, 55, 56, 74, 96, 22, 62, 20, 70, 68, 90, 50];
  tree := TSet_int.Create;

  for i := 0 to High(arr) do
  begin
    tree.Add(arr[i]);
  end;

  TArrayUtils_int.Print(tree.ToArray);

  for i := 0 to High(arr) do
  begin
    tree.Remove(arr[i]);
  end;
end;

end.
