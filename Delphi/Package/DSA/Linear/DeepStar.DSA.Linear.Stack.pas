unit DeepStar.DSA.Linear.Stack;

interface

uses
  DeepStar.DSA.Interfaces,
  DeepStar.DSA.Linear.DoubleLinkedList;

type
  TStack<T> = class(TInterfacedObject, IStack<T>)
  private type
    TDoubleLinkedList = TDoubleLinkedList<T>;

  private
    _data: TDoubleLinkedList;

  public
    constructor Create;
    destructor Destroy; override;

    function Count: integer;
    function IsEmpty: boolean;
    procedure Push(e: T);
    function Pop: T;
    function Peek: T;
  end;

implementation

{ TStack }

constructor TStack<T>.Create;
begin
  _data := TDoubleLinkedList.Create;
end;

function TStack<T>.Count: integer;
begin
  Result := _data.Count;
end;

destructor TStack<T>.Destroy;
begin
  _data.Free;
  inherited Destroy;
end;

function TStack<T>.IsEmpty: boolean;
begin
  Result := _data.IsEmpty;
end;

function TStack<T>.Peek: T;
begin
  Result := _data.GetLast;
end;

function TStack<T>.Pop: T;
begin
  Result := _data.RemoveLast;
end;

procedure TStack<T>.Push(e: T);
begin
  _data.AddLast(e);
end;

end.
