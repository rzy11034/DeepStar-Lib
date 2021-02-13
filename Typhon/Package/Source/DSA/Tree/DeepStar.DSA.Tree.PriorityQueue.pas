unit DeepStar.DSA.Tree.PriorityQueue;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.DSA.Interfaces,
  DeepStar.DSA.Tree.Heap;

type
  THeapkind = DeepStar.DSA.Tree.Heap.THeapkind;

  generic TPriorityQueue<T> = class(TInterfacedObject, specialize IQueue<T>)
  public type
    THeap = specialize THeap<T>;
    TImpl = specialize TImpl<T>;
    ICmp = TImpl.ICmp;
    TCmp = TImpl.TCmp;

  private
    _heap: THeap;

  public
    constructor Create(heapkind: THeapkind = THeapkind.Min);
    constructor Create(cmp: ICmp; heapkind: THeapkind = THeapkind.Min);
    destructor Destroy; override;

    function Count: integer;
    function IsEmpty: boolean;
    procedure EnQueue(e: T);
    function DeQueue: T;
    function Peek: T;
  end;

implementation

{ TPriorityQueue }

constructor TPriorityQueue.Create(cmp: ICmp; heapkind: THeapkind);
begin
  _heap := THeap.Create(cmp, 10, heapkind);
end;

constructor TPriorityQueue.Create(heapkind: THeapkind);
begin
  Create(TImpl.TCmp.Default, heapkind);
end;

function TPriorityQueue.Count: integer;
begin
  Result := _heap.Count;
end;

function TPriorityQueue.DeQueue: T;
begin
  Result := _heap.ExtractFirst;
end;

destructor TPriorityQueue.Destroy;
begin
  FreeAndNil(_heap);
  inherited Destroy;
end;

procedure TPriorityQueue.EnQueue(e: T);
begin
  _heap.Add(e);
end;

function TPriorityQueue.IsEmpty: boolean;
begin
  Result := _heap.IsEmpty;
end;

function TPriorityQueue.Peek: T;
begin
  Result := _heap.FindFirst;
end;

end.
