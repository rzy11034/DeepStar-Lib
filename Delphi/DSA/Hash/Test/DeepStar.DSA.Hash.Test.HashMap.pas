unit DeepStar.DSA.Hash.Test.HashMap;

interface

uses
  System.SysUtils,
  DeepStar.DSA.Hash.HashMap,
  DeepStar.DSA.Interfaces,
  LQA.Utils;

procedure Main;

implementation

type
  TMap = THashMap<integer, integer>;
  TPtr_int = TPtr_V<integer>;

procedure Main;
var
  map, mm: TMap;
  i: integer;
  arr: TArr_int;
begin
  map := TMap.Create;
  arr := [55, 55, 56, 74, 96, 22, 62, 20, 70, 68, 90, 50];

  for i := 0 to high(arr) do
  begin
    map.Add(arr[i], 0);
  end;

  WriteLn(map.Remove(55).Value);

  WriteLn(map.Count);
  WriteLn(map.IsEmpty);
  TArrayUtils_int.Print(map.Keys);
  TArrayUtils_int.Print(map.Values);

  DrawLineBlockEnd;

  mm := map.Clone;
  TArrayUtils_int.Print(mm.Keys);
  TArrayUtils_int.Print(mm.Values);

  DrawLineBlockEnd;

  map.AddAll(mm);
  WriteLn(map.ContainsKey(99));
  TArrayUtils_int.Print(map.Keys);
  TArrayUtils_int.Print(map.Values);

  DrawLineBlockEnd;

  map.Clear;
  WriteLn(map.Count);
  WriteLn(map.IsEmpty);
  TArrayUtils_int.Print(map.Keys);
  TArrayUtils_int.Print(map.Values);
end;

end.
