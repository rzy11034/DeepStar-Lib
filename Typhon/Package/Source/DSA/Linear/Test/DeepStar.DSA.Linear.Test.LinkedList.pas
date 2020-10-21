unit DeepStar.DSA.Linear.Test.LinkedList;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.DSA.Linear.LinkedList,
  DeepStar.DSA.Interfaces,
  DeepStar.Utils;

procedure Main;

implementation

type
  TLinkedList = specialize TLinkedList<integer>;
  TImpl = specialize TImpl<integer>;

procedure Main;
var
  al: TLinkedList;
  a: TImpl.TArr;
begin
  a := [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20];
  al := TLinkedList.Create;
  al.AddRange(a);
  al.RemoveFirst;
  al.RemoveLast;
  al.RemoveElement(10);
  writeln('Main ', al.ToString);
  writeln('Main ', al.Remove(3));
  writeln('Main ', al.Count);
  DrawLineBlockEnd;

  al.AddLast(100);
  writeln('Main ', al.ToString);
  writeln('Main ', al.Count);
  DrawLineBlockEnd;

  al.Add(1, 100);
  writeln('Main ', al.ToString);
  writeln('Main ', al.Count);
  DrawLineBlockEnd;

  al.RemoveElement(100);
  writeln('Main ', al.ToString);
  writeln('Main ', al.Count);
  DrawLineBlockEnd;

  al.Clear;
  writeln('Main ', al.ToString);
  writeln('Main ', al.Count);

  al.Free;
end;

end.
