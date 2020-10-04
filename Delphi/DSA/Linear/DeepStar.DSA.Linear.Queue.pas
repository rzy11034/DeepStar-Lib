unit DeepStar.DSA.Linear.Queue;

interface

uses
  DeepStar.DSA.Interfaces,
  DeepStar.DSA.Linear.DoubleLinkedList;

type
  TQueue<T> = class(TInterfacedObject, IQueue<T>)
  private type
    TDoubleLinkedList = TDoubleLinkedList<T>;

  private
    _data: TDoubleLinkedList;

  public
    constructor Create;
    destructor Destroy; override;
    function Count: integer;
    function IsEmpty: boolean;
    procedure EnQueue(e: T);
    function DeQueue: T;
    function Peek: T;
  end;

implementation

{ TQueue<T> }

constructor TQueue<T>.Create;
begin
  _data := TDoubleLinkedList.Create;
end;

function TQueue<T>.Count: integer;
begin
  Result := _data.Count;
end;

function TQueue<T>.DeQueue: T;
begin
  Result := _data.RemoveFirst;
end;

destructor TQueue<T>.Destroy;
begin
  _data.Free;
  inherited Destroy;
end;

procedure TQueue<T>.EnQueue(e: T);
begin
  _data.AddLast(e);
end;

function TQueue<T>.IsEmpty: boolean;
begin
  Result := _data.IsEmpty;
end;

function TQueue<T>.Peek: T;
begin
  Result := _data.GetFirst;
end;

end.
