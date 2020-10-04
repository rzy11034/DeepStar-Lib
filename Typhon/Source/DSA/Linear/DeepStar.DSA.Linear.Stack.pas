unit DeepStar.DSA.Linear.Stack;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.DSA.Interfaces,
  DeepStar.DSA.Linear.DoubleLinkedList;

type
  generic TStack<T> = class(TInterfacedObject, specialize IStack<T>)
  private type
    TDoubleLinkedList = specialize TDoubleLinkedList<T>;

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

constructor TStack.Create;
begin
  _data := TDoubleLinkedList.Create;
end;

function TStack.Count: integer;
begin
  Result := _data.Count;
end;

destructor TStack.Destroy;
begin
  _data.Free;
  inherited Destroy;
end;

function TStack.IsEmpty: boolean;
begin
  Result := _data.IsEmpty;
end;

function TStack.Peek: T;
begin
  Result := _data.GetLast;
end;

function TStack.Pop: T;
begin
  Result := _data.RemoveLast;
end;

procedure TStack.Push(e: T);
begin
  _data.AddLast(e);
end;

end.
