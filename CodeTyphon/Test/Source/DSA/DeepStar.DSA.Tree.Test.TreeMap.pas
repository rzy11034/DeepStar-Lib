unit DeepStar.DSA.Tree.Test.TreeMap;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.DSA.Tree.TreeMap,
  DeepStar.Utils;

procedure Main;

implementation

type
  TMap_int_int = specialize TTreeMap<integer, integer>;

procedure Main;
var
  tree: TMap_int_int;
  arr: TArr_int;
  i: integer;
begin
  arr := [55, 55, 56, 74, 96, 22, 62, 20, 70, 68, 90, 50];
  tree := TMap_int_int.Create;

  for i := 0 to High(arr) do
  begin
    tree.Add(arr[i], 0);
  end;

  TArrayUtils_int.Print(tree.Keys);
  i := tree.Floor(33).Value;
  i := tree.Ceiling(44).Value;

  for i := 0 to High(arr) do
  begin
    tree.Remove(arr[i]);
  end;

  tree.Free;
end;

end.

