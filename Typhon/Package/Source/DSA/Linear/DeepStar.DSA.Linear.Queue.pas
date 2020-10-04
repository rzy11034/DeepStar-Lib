unit DeepStar.DSA.Linear.Queue;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.DSA.Interfaces,
  DeepStar.DSA.Linear.DoubleLinkedList;

type
  generic TQueue<T> = class(TInterfacedObject, specialize IQueue<T>)
  private type
    TDoubleLinkedList = specialize TDoubleLinkedList<T>;

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

{ TQueue }

constructor TQueue.Create;
begin
  _data := TDoubleLinkedList.Create;
end;

function TQueue.Count: integer;
begin
  Result := _data.Count;
end;

function TQueue.DeQueue: T;
begin
  Result := _data.RemoveFirst;
end;

destructor TQueue.Destroy;
begin
  _data.Free;
  inherited Destroy;
end;

procedure TQueue.EnQueue(e: T);
begin
  _data.AddLast(e);
end;

function TQueue.IsEmpty: boolean;
begin
  Result := _data.IsEmpty;
end;

function TQueue.Peek: T;
begin
  Result := _data.GetFirst;
end;

end.
