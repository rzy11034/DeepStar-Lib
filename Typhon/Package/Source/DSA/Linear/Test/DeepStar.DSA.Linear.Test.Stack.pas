unit DeepStar.DSA.Linear.Test.Stack;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,  DeepStar.Utils.UString,
  DeepStar.DSA.Linear.Stack;

procedure Main;

implementation

type
  TStack = specialize TStack<UChar>;

procedure Main;
var
  s: TStack;
  i: integer;
begin
  s := TStack.Create;

  for i := 0 to 4 do
  begin
    s.Push('A');
  end;

  //while not s.IsEmpty do
  //begin
  //  WriteLn(s.Count, ' - ', s.Peek, ' -> ', s.Pop);
  //end;

  WriteLn(s.Count);
  s.Free;
end;

end.
