unit DeepStar.DSA.Linear.Test.ArrayList;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.DSA.Linear.ArrayList,
  DeepStar.DSA.Interfaces,
  DeepStar.Utils.UString,
  DeepStar.Utils;

procedure Main;

implementation

type
  TImpl = specialize TImpl<integer>;
  TArrayList = specialize TArrayList<TImpl>;

function cmp(constref a, b: integer): integer;
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
  t: TImpl;
  a: TImpl.TArr;
  i: integer;
begin
  //a := [1, 2, 6, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 8, 20];
  //al := TArrayList.Create(@cmp);
  //al.AddRange(a);
  //
  //al.Sort;
  //TArrayUtils_int.Print(al.ToArray);
  //al.Free;

  al := TArrayList.Create;
  for i := 0 to 9 do
  begin
    al.AddLast(TImpl.Create);
  end;

  for i := 0 to al.Count-1 do
  begin
    t:=al.RemoveLast;
    t.Free;
  end;

  //al.Clear;
  al.Free;
end;

end.
