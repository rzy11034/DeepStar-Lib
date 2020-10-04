unit DeepStar.DSA.Tree.Test.TreeMap;

interface

uses
  System.SysUtils,
  DeepStar.DSA.Tree.TreeMap,
  LQA.Utils;

procedure Main;

implementation

type
  TMap_int_int = TTreeMap<integer, integer>;

procedure Main;
var
  Tree: TMap_int_int;
  arr: TArr_int;
  i: integer;
begin
  arr := [55, 55, 56, 74, 96, 22, 62, 20, 70, 68, 90, 50];
  Tree := TMap_int_int.Create;

  for i := 0 to high(arr) do
  begin
    Tree.Add(arr[i], 0);
  end;

  TArrayUtils_int.Print(Tree.Keys);

  for i := 0 to high(arr) do
  begin
    Tree.Remove(arr[i]);
  end;
end;

end.
