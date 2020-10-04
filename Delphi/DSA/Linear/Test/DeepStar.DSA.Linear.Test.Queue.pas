unit DeepStar.DSA.Linear.Test.Queue;

interface

uses
  System.SysUtils,
  DeepStar.DSA.Linear.Queue;

procedure Main;

implementation

type
  TQueue = TQueue<integer>;

procedure Main;
var
  q: TQueue;
  i: integer;
begin
  q := TQueue.Create;

  for i := 0 to 4 do
  begin
    q.EnQueue(i);

  end;

  while not q.IsEmpty do
  begin
    WriteLn(q.Count, ' - ', q.Peek, ' -> ', q.DeQueue);
  end;

  WriteLn(q.Count);
end;

end.
