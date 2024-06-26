﻿unit DeepStar.DSA.Tree.Test.AVLTree;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.DSA.Tree.AVLTree;

procedure Main;

implementation

uses DeepStar.Utils;

type
  TAVL_int_int = specialize TAVLTree<integer, integer>;

procedure Main;
var
  tree: TAVL_int_int;
  arr: TArr_int;
  i: integer;
begin
  arr := [13, 14, 15, 12, 11, 17, 16, 8, 9, 1];
  tree := TAVL_int_int.Create;

  for i := 0 to High(arr) do
  begin
    tree.Add(arr[i], 0);
  end;

  TArrayUtils_int.Print(tree.Keys);
  for i := High(arr) downto 0 do
  begin
    tree.Remove(arr[i]);
  end;

  tree.Free;
end;

end.
