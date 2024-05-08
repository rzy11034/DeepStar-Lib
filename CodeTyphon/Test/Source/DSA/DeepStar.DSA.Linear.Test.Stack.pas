unit DeepStar.DSA.Linear.Test.Stack;

{$mode objfpc}{$H+}
{$ModeSwitch unicodestrings}{$J-}

interface

uses
  Classes,
  SysUtils,
  DeepStar.DSA.Interfaces,
  DeepStar.DSA.Linear.Stack;

procedure Main;

implementation

uses
  DeepStar.Utils;

type
  TStack = specialize TStack<char>;

procedure Main;
var
  s: TStack;
  i: integer;
begin
  s := TStack.Create;

  for i := 0 to 4 do
  begin
    s.Push(char(Ord('A') + i));
  end;

  while not s.IsEmpty do
  begin
    WriteLn(s.Count, ' - ', s.Peek, ' -> ', s.Pop);
  end;

  WriteLn(s.Count);
  s.Free;
end;

end.
