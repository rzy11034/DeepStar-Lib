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

procedure Print(p: TSet_int.TPtrValue_T);
begin
  if p = nil then
    WriteLn('nil')
  else
    WriteLn(p.Value);
end;

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

  Print(tree.Ceiling(0));
  Print(tree.Ceiling(60));
  Print(tree.Ceiling(91));
  Print(tree.Ceiling(70));
  Print(tree.Ceiling(100));

  DrawLineBlockEnd;

  Print(tree.Floor(0));
  Print(tree.Floor(60));
  Print(tree.Floor(91));
  Print(tree.Floor(70));
  Print(tree.Floor(100));


  for i := 0 to High(arr) do
  begin
    tree.Remove(arr[i]);
  end;
end;

end.
