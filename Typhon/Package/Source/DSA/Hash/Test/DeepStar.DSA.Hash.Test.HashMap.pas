unit DeepStar.DSA.Hash.Test.HashMap;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.DSA.Hash.HashMap,
  DeepStar.DSA.Interfaces,
  DeepStar.Utils;

procedure Main;

implementation

type
  TMap = specialize THashMap<integer, integer>;
  IPtrValue = specialize IPtrValue<integer>;

procedure Main;
var
  map, mm: TMap;
  i: integer;
  arr: TArr_int;
  p: IPtrValue;
begin
  //mm := nil;
  map := TMap.Create;
  arr := [55, 55, 56, 74, 96, 22, 62, 20, 70, 68, 90, 50];
  //arr := [55, 50];

  for i := 0 to High(arr) do
  begin
    p := map.Add(arr[i], i + 100);

    if p <> nil then
    begin
      WriteLn(p.Value);
    end
    else
      WriteLn('nil');
  end;

  map[55] := 99;

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

  map.Free;
  mm.Free;
end;

end.
