unit DeepStar.DSA.Linear.Test.DoubleLinkedList;

interface

uses
  System.SysUtils,
  DeepStar.DSA.Interfaces,
  LQA.Utils;

procedure Main;

implementation

uses
  DeepStar.DSA.Linear.DoubleLinkedList;

type
  TImpl = TImpl<integer>;
  TList = TDoubleLinkedList<integer>;
  //TList = TDoubleLinkedList;

procedure Main;
var
  a: TImpl.TArr;
  l: TList;
begin
  a := [1, 2, 3, 4, 5, 6, 7, 8, 9];
  l := TList.Create;

  l.AddRange(a);

  writeln(l.Contains(5));
  WriteLn(l.IndexOf(10));

  l.AddLast(100);
  l.AddFirst(100);
  TArrayUtils_int.Print(l.ToArray);

  l.Remove(5);
  l.RemoveFirst;
  l.RemoveLast;
  TArrayUtils_int.Print(l.ToArray);
  l.RemoveLast;
  l.RemoveFirst;
  TArrayUtils_int.Print(l.ToArray);

  l.Clear;
  TArrayUtils_int.Print(l.ToArray);

  a := [1, 2, 1, 3, 1];
  l.AddRange(a);
  l.RemoveElement(1);
  TArrayUtils_int.Print(l.ToArray);

  l.Clear;
  a := [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
  l.AddRange(a);
  l.Items[5] := 100;
  TArrayUtils_int.Print(l.ToArray);
  WriteLn(l.Items[5]);
  WriteLn(l.ToString);

end;

end.
