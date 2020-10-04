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
  private type
    THeap = specialize THeap<T>;
    TImpl = specialize TImpl<T>;
    ICmp = TImpl.ICmp;

  private
    _heap: THeap;
    function __getCmp: ICmp;
    procedure __setCmp(const newComparer: ICmp);

  public
    constructor Create(heapkind: THeapkind = THeapkind.Min);
    constructor Create(cmp: ICmp; heapkind: THeapkind = THeapkind.Min);
    destructor Destroy; override;

    function Count: integer;
    function IsEmpty: boolean;
    procedure EnQueue(e: T);
    function DeQueue: T;
    function Peek: T;

    property Comparer: ICmp read __getCmp write __setCmp;
  end;

implementation

{ TPriorityQueue }

constructor TPriorityQueue.Create(cmp: ICmp; heapkind: THeapkind);
begin
  _heap := THeap.Create(10, heapkind);
  _heap.Comparer := cmp;
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

function TPriorityQueue.__getCmp: ICmp;
begin
  Result := _heap.Comparer;
end;

procedure TPriorityQueue.__setCmp(const newComparer: ICmp);
begin
  _heap.Comparer := newComparer;
end;

end.
