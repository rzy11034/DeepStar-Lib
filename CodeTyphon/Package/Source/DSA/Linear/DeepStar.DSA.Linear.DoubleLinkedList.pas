﻿unit DeepStar.DSA.Linear.DoubleLinkedList;

{$mode objfpc}{$H+}
{$ModeSwitch unicodestrings}{$J-}

interface

uses
  Classes,
  SysUtils,
  Rtti,
  DeepStar.DSA.Interfaces;

type
  generic TDoubleLinkedList<T> = class(TInterfacedObject, specialize IList<T>)
  public type
    TImpl = specialize TImpl<T>;
    ICmp = TImpl.ICmp;
    TCmp = TImpl.TCmp;

    TNode = class(TObject)
    public
      E: T;
      Prev: TNode;
      Next: TNode;

      constructor Create;
      constructor Create(newE: T);
      destructor Destroy; override;
    end;

  private
    _size: integer;
    _dummyhead: TNode;
    _dummyTail: TNode;
    _cmp: TImpl.ICmp;

    procedure __SetComparer(const newComparer: TImpl.ICmp);
    procedure __Swap(var a, b: T);

  public
    constructor Create;
    constructor Create(const arr: array of T);
    constructor Create(comparisonFunc: TImpl.TComparisonFuncs);
    constructor Create(onComparison: TImpl.TOnComparisons);
    constructor Create(cmp: TImpl.ICmp);
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
    function ToString: string; reintroduce;
    procedure Add(index: integer; e: T);
    procedure AddFirst(e: T);
    procedure AddLast(e: T);
    procedure AddRange(const arr: array of T);
    procedure AddRange(const arr: array of T; aIndex, aCount: integer);
    procedure Clear;
    procedure RemoveElement(e: T);
    procedure SetItem(index: integer; e: T);
    procedure Reverse;

    property Count: integer read GetSize;
    property Comparer: TImpl.ICmp write __SetComparer;
    property Items[i: integer]: T read GetItem write SetItem; default;
  end;

implementation

{ TDoubleLinkedList.TNode }

constructor TDoubleLinkedList.TNode.Create(newE: T);
begin
  E := newE;
  Prev := nil;
  Next := nil;
end;

constructor TDoubleLinkedList.TNode.Create;
begin
  Self.Create(Default(T));
end;

destructor TDoubleLinkedList.TNode.Destroy;
begin
  inherited Destroy;
end;

{ TDoubleLinkedList }

constructor TDoubleLinkedList.Create(const arr: array of T);
begin
  Self.Create;
  Self.AddRange(arr);
end;

constructor TDoubleLinkedList.Create(cmp: TImpl.ICmp);
begin
  Self.Create;
  _cmp := cmp;
end;

constructor TDoubleLinkedList.Create(comparisonFunc: TImpl.TComparisonFuncs);
begin
  Self.Create;
  _cmp := TCmp.Construct(comparisonFunc);
end;

constructor TDoubleLinkedList.Create(onComparison: TImpl.TOnComparisons);
begin
  Self.Create;
  _cmp := TCmp.Construct(onComparison);
end;

constructor TDoubleLinkedList.Create;
begin
  _dummyhead := TNode.Create;
  _dummyTail := TNode.Create;
  _dummyhead.Next := _dummyTail;
  _dummyTail.Prev := _dummyhead;
  _size := 0;
  _cmp := TImpl.TCmp.Default;
end;

procedure TDoubleLinkedList.Add(index: integer; e: T);
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
    _dummyTail.Prev := tmp;
    tmp.Prev := _dummyhead;
    tmp.Next := _dummyTail;
    _size += 1;
    Exit;
  end;

  mid := _size div 2;

  if (index >= mid) and (_size > 3) then
  begin
    cur := _dummyTail.Prev;
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

  _size += 1;
end;

procedure TDoubleLinkedList.AddFirst(e: T);
begin
  Self.Add(0, e);
end;

procedure TDoubleLinkedList.AddLast(e: T);
begin
  Self.Add(_size, e);
end;

procedure TDoubleLinkedList.AddRange(const arr: array of T);
var
  i: integer;
begin
  for i := 0 to High(arr) do
  begin
    Self.AddLast(arr[i]);
  end;
end;

procedure TDoubleLinkedList.AddRange(const arr: array of T; aIndex, aCount: integer);
var
  i: integer;
begin
  if (aIndex < 0) or (aIndex + aCount > Length(arr)) then
    raise Exception.Create('Add failed.');

  for i := aIndex to aCount - 1 do
  begin
    Self.AddLast(arr[i]);
  end;
end;

procedure TDoubleLinkedList.Clear;
begin
  while not IsEmpty do
  begin
    Self.RemoveLast;
  end;
end;

function TDoubleLinkedList.Contains(e: T): boolean;
var
  cur: TNode;
begin
  cur := _dummyHead.Next;

  while cur <> _dummyTail do
  begin
    if _cmp.Compare(cur.E, e) = 0 then
      Exit(true);

    cur := cur.Next;
  end;

  Result := false;
end;

destructor TDoubleLinkedList.Destroy;
begin
  Self.Clear;
  FreeAndNil(_dummyTail);
  FreeAndNil(_dummyhead);
  inherited Destroy;
end;

function TDoubleLinkedList.GetFirst: T;
begin
  Result := GetItem(0);
end;

function TDoubleLinkedList.GetItem(index: integer): T;
var
  mid, i: integer;
  cur: TNode;
begin
  if (index < 0) or (index >= _size) then
    raise Exception.Create('GetItem failed. Index is Illegal.');

  mid := _size div 2;

  if index >= mid then
  begin
    cur := _dummyTail.Prev;
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

function TDoubleLinkedList.GetLast: T;
begin
  Result := GetItem(_size - 1);
end;

function TDoubleLinkedList.GetSize: integer;
begin
  Result := _size;
end;

function TDoubleLinkedList.IndexOf(e: T): integer;
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
    i += 1;
  end;

  Result := -1;
end;

function TDoubleLinkedList.IsEmpty: boolean;
begin
  Result := _size = 0;
end;

function TDoubleLinkedList.Remove(index: integer): T;
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
    del := _dummyTail.Prev;
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
  _size -= 1;

  Result := res;
end;

procedure TDoubleLinkedList.RemoveElement(e: T);
var
  cur, del: TNode;
begin
  cur := _dummyhead.Next;

  while cur <> _dummyTail do
  begin
    if _cmp.Compare(cur.E, e) = 0 then
    begin
      del := cur;
      cur.Prev.Next := del.Next;
      cur.Next.Prev := del.Prev;
      cur := cur.Next;
      FreeAndNil(del);
      _size -= 1;
      Continue;
    end;

    cur := cur.Next;
  end;
end;

function TDoubleLinkedList.RemoveFirst: T;
begin
  Result := Remove(0);
end;

function TDoubleLinkedList.RemoveLast: T;
begin
  Result := Remove(_size - 1);
end;

procedure TDoubleLinkedList.Reverse;
var
  head, tail: TNode;
  l, r: integer;
begin
  if _size <= 1 then Exit;

  l := 0;
  r := _size - 1;
  head := _dummyhead.Next;
  tail := _dummyTail.Prev;

  while l < r do
  begin
    __Swap(head.E, tail.E);
    head := head.Next;
    tail := tail.Prev;

    l += 1;
    r -= 1;
  end;
end;

procedure TDoubleLinkedList.SetItem(index: integer; e: T);
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
    cur := _dummyTail.Prev;
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

function TDoubleLinkedList.ToArray: TImpl.TArr;
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

function TDoubleLinkedList.ToString: string;
var
  sb: TStringBuilder;
  cur: TNode;
  e: T;
  Value: TValue;
begin
  sb := TStringBuilder.Create;
  try
    cur := _dummyHead.Next;

    while cur <> _dummyTail do
    begin
      e := cur.E;
      TValue.Make(@e, TypeInfo(T), Value);

      sb.Append(string(Value.ToString) + ' -> ');
      cur := cur.Next;
    end;

    sb.Append('nil');

    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

procedure TDoubleLinkedList.__SetComparer(const newComparer: TImpl.ICmp);
begin
  _cmp := newComparer;
end;

procedure TDoubleLinkedList.__Swap(var a, b: T);
var
  temp: T;
begin
  temp := a;
  a := b;
  b := temp;
end;

end.
