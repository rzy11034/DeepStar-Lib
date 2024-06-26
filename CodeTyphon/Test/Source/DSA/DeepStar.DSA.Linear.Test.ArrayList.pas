﻿unit DeepStar.DSA.Linear.Test.ArrayList;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.DSA.Interfaces,
  DeepStar.DSA.Linear.ArrayList;

procedure Main;

implementation

uses
  DeepStar.Utils;

type
  TImpl = specialize TImpl<integer>;
  TArrayList = specialize TArrayList<integer>;

function cmp(const a, b: integer): integer;
var
  res: integer;
begin
  if a > b then
    res := -1
  else if a = b then
    res := 0
  else
    res := 1;

  Result := res;
end;

procedure Main;
var
  al: TArrayList;
  a: TImpl.TArr;
  i: Integer;
begin
  a := [1, 2, 6, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 8, 20];
  al := TArrayList.Create(@cmp);
  al.AddRange(a);

  al.Sort;
  TArrayUtils_int.Print(al.ToArray);
  al.Free;

  ///////////////////////////
  al := TArrayList.Create(0);

  for i := 0 to 9999 do
  begin
    al.AddLast(1);
  end;

  for i := 0 to 9999 do
  begin
    al.RemoveLast;
  end;

  al.Free;
end;

end.
