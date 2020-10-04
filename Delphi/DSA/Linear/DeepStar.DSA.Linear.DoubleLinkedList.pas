unit DeepStar.DSA.Linear.DoubleLinkedList;

interface

uses
  System.SysUtils,
  System.Rtti,
  DeepStar.DSA.Interfaces,
  DeepStar.Utils.UString;

type
  TDoubleLinkedList<T> = class(TInterfacedObject, IList<T>)
  private type
    TImpl = TImpl<T>;
    TArr = TImpl.TArr;

  public type
    TNode = class(TObject)
    public
      E: T;
      Prev: TNode;
      Next: TNode;

      constructor Create; overload;
      constructor Create(newE: T); overload;
      destructor Destroy; override;
    end;

  private
    _size: integer;
    _dummyhead: TNode;
    _tail: TNode;
    _cmp: TImpl.ICmp;

  public
    constructor Create;
    destructor Destroy; override;

    function Contains(e: T): boolean;
    function GetFirst: T;
    function GetItem(index: integer): T;
    function GetLast: T;
    function GetSize: integer;
    function IndexOf(e: T): integer;
    function IsEmpty: boolean;
    function Remove(index: integer): T;
    function RemoveFirst: T;
    function RemoveLast: T;
    function ToArray: TImpl.TArr;
    function ToString: UString; reintroduce;
    procedure Add(index: integer; e: T);
    procedure AddFirst(e: T);
    procedure AddLast(e: T);
    procedure AddRange(const arr: TArr);
    procedure Clear;
    procedure RemoveElement(e: T);
    procedure SetItem(index: integer; e: T);

    property Count: integer read GetSize;
    property Comparer: TImpl.ICmp read _cmp write _cmp;
    property Items[i: integer]: T read GetItem write SetItem; default;
  end;

implementation

{ TDoubleLinkedList.TNode }

constructor TDoubleLinkedList<T>.TNode.Create(newE: T);
begin
  E := newE;
  Prev := nil;
  Next := nil;
end;

constructor TDoubleLinkedList<T>.TNode.Create;
begin
  Self.Create(default (T));
end;

destructor TDoubleLinkedList<T>.TNode.Destroy;
begin
  inherited Destroy;
end;

{ TDoubleLinkedList<T> }

constructor TDoubleLinkedList<T>.Create;
begin
  _dummyhead := TNode.Create;
  _tail := TNode.Create;
  _dummyhead.Next := _tail;
  _tail.Prev := _dummyhead;
  _size := 0;
  _cmp := TImpl.TCmp.Default;
end;

procedure TDoubleLinkedList<T>.Add(index: integer; e: T);
var
  cur, tmp: TNode;
  mid, i: integer;
begin
  if (index < 0) or (index > _size) then
    raise Exception.Create('Add failed. Index is Illegal.');

  if IsEmpty then
  begin
    tmp := TNode.Create(e);
    _dummyhead.Next := tmp;
    _tail.Prev := tmp;
    tmp.Prev := _dummyhead;
    tmp.Next := _tail;
    Inc(_size);
    Exit;
  end;

  mid := _size div 2;

  if index >= mid then
  begin
    cur := _tail.Prev;
    for i := _size - 1 downto index + 1 do
      cur := cur.Prev;

    tmp := TNode.Create(e);
    tmp.prev := cur;
    tmp.Next := cur.Next;
    cur.Next.Prev := tmp;
    cur.Next := tmp;
  end
  else
  begin
    cur := _dummyhead.Next;
    for i := 0 to index - 1 do
      cur := cur.Next;

    tmp := TNode.Create(e);
    tmp.prev := cur.Prev;
    tmp.Next := cur;
    cur.Prev.Next := tmp;
    cur.Prev := tmp;
  end;

  Inc(_size);
end;

procedure TDoubleLinkedList<T>.AddFirst(e: T);
begin
  Self.Add(0, e);
end;

procedure TDoubleLinkedList<T>.AddLast(e: T);
begin
  Self.Add(_size, e);
end;

procedure TDoubleLinkedList<T>.AddRange(const arr: TArr);
var
  i: integer;
begin
  for i := 0 to High(arr) do
  begin
    Self.AddLast(arr[i]);
  end;
end;

procedure TDoubleLinkedList<T>.Clear;
begin
  while not IsEmpty do
  begin
    Self.RemoveLast;
  end;
end;

function TDoubleLinkedList<T>.Contains(e: T): boolean;
var
  cur: TNode;
begin
  cur := _dummyHead.Next;

  while cur <> _tail do
  begin
    if _cmp.Compare(cur.E, e) = 0 then
      Exit(true);

    cur := cur.Next;
  end;

  Result := false;
end;

destructor TDoubleLinkedList<T>.Destroy;
begin
  Self.Clear;
  FreeAndNil(_tail);
  FreeAndNil(_dummyhead);
  inherited Destroy;
end;

function TDoubleLinkedList<T>.GetFirst: T;
begin
  Result := GetItem(0);
end;

function TDoubleLinkedList<T>.GetItem(index: integer): T;
var
  mid, i: integer;
  cur: TNode;
begin
  if (index < 0) or (index >= _size) then
    raise Exception.Create('GetItem failed. Index is Illegal.');

  mid := _size div 2;

  if index >= mid then
  begin
    cur := _tail.Prev;
    for i := _size - 1 downto index + 1 do
      cur := cur.Prev;
  end
  else
  begin
    cur := _dummyhead.Next;
    for i := 0 to index - 1 do
      cur := cur.Next;
  end;

  Result := cur.E;
end;

function TDoubleLinkedList<T>.GetLast: T;
begin
  Result := GetItem(_size - 1);
end;

function TDoubleLinkedList<T>.GetSize: integer;
begin
  Result := _size;
end;

function TDoubleLinkedList<T>.IndexOf(e: T): integer;
var
  i: integer;
  cur: TNode;
begin
  i := 0;
  cur := _dummyHead.Next;

  while cur <> nil do
  begin
    if _cmp.Compare(cur.E, e) = 0 then
      Exit(i);

    cur := cur.Next;
    Inc(i);
  end;

  Result := -1;
end;

function TDoubleLinkedList<T>.IsEmpty: boolean;
begin
  Result := _size = 0;
end;

function TDoubleLinkedList<T>.Remove(index: integer): T;
var
  del: TNode;
  i, mid: integer;
  res: T;
begin
  if (index < 0) or (index >= _size) then
    raise Exception.Create('Remove failed. Index is Illegal.');

  mid := _size div 2;

  if index >= mid then
  begin
    del := _tail.Prev;
    for i := _size - 1 downto index + 1 do
      del := del.Prev;
  end
  else
  begin
    del := _dummyhead.Next;
    for i := 0 to index - 1 do
      del := del.Next;
  end;

  del.Prev.Next := del.Next;
  del.Next.Prev := del.Prev;
  res := del.E;
  FreeAndNil(del);
  Dec(_size);

  Result := res;
end;

procedure TDoubleLinkedList<T>.RemoveElement(e: T);
var
  cur, del: TNode;
begin
  cur := _dummyhead.Next;

  while cur <> _tail do
  begin
    if _cmp.Compare(cur.E, e) = 0 then
    begin
      del := cur;
      cur.Prev.Next := del.Next;
      cur.Next.Prev := del.Prev;
      cur := cur.Next;
      FreeAndNil(del);
      Dec(_size);
      Continue;
    end;

    cur := cur.Next;
  end;
end;

function TDoubleLinkedList<T>.RemoveFirst: T;
begin
  Result := Remove(0);
end;

function TDoubleLinkedList<T>.RemoveLast: T;
begin
  Result := Remove(_size - 1);
end;

procedure TDoubleLinkedList<T>.SetItem(index: integer; e: T);
var
  mid: integer;
  cur: TNode;
  i: integer;
begin
  if (index < 0) or (index >= _size) then
    raise Exception.Create('SetItem failed. Index is Illegal.');

  mid := _size div 2;

  if index >= mid then
  begin
    cur := _tail.Prev;
    for i := _size - 1 downto index + 1 do
      cur := cur.Prev;
  end
  else
  begin
    cur := _dummyhead.Next;
    for i := 0 to index - 1 do
      cur := cur.Next;
  end;

  cur.E := e;
end;

function TDoubleLinkedList<T>.ToArray: TImpl.TArr;
var
  res: TImpl.TArr;
  cur: TNode;
  i: integer;
begin
  SetLength(res, Count);
  cur := _dummyHead.Next;

  for i := 0 to High(res) do
  begin
    res[i] := cur.E;
    cur := cur.Next;
  end;

  Result := res;
end;

function TDoubleLinkedList<T>.ToString: UString;
var
  sb: TStringBuilder;
  cur: TNode;
  e: T;
  Value: TValue;
begin
  sb := TStringBuilder.Create;
  try
    cur := _dummyHead.Next;

    while cur <> _tail do
    begin
      e := cur.E;
      TValue.Make(@e, TypeInfo(T), Value);

      sb.Append(Value.ToString + ' -> ');
      cur := cur.Next;
    end;

    sb.Append('nil');

    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

end.
