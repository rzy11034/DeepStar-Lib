unit DeepStar.DSA.Hash.HashMap;

interface

uses
  System.SysUtils,
  System.Rtti,
  DeepStar.DSA.Interfaces,
  DeepStar.DSA.Linear.LinkedList;

type
  THashMap<K, V> = class(TInterfacedObject, IMap<K, V>)
  public type
    TPair = class
      Key: K;
      Value: V;
      constructor Create(newKey: K; newValue: V);
    end;

  private type
    TPtr_V = TPtr_V<V>;
    TImpl_K = TImpl<K>;
    TImpl_V = TImpl<V>;
    TImpl_TPair = TImpl<TPair>;
    TLinkedList_K = TLinkedList<K>;
    TLinkedList_V = TLinkedList<V>;
    TLinkedList_TPair = TLinkedList<TPair>;

    THashMap_K_V = THashMap<K, V>;

  private
    _data: array of TLinkedList_TPair;
    _capacity: integer;
    _size: integer;
    _cmp_K: TImpl_K.ICmp;
    _cmp_V: TImpl_V.ICmp;

    function __getItem(Key: K): TPtr_V;
    function __hash(Key: K): integer;

  public
    constructor Create(newCapacity: integer = 31);
    destructor Destroy; override;

    function Add(Key: K; Value: V): TPtr_V;
    function Clone: THashMap_K_V;
    function ContainsKey(Key: K): boolean;
    function ContainsValue(Value: V): boolean;
    function Count: integer;
    function GetItem(Key: K): V;
    function IsEmpty: boolean;
    function Keys: TImpl_K.TArr;
    function Pairs: TImpl_TPair.TArr;
    function Remove(Key: K): TPtr_V;
    function Values: TImpl_V.TArr;
    procedure AddAll(map: THashMap_K_V);
    procedure Clear;
    procedure SetItem(Key: K; Value: V);

    property Comparer_K: TImpl_K.ICmp read _cmp_K write _cmp_K;
    property Comparer_V: TImpl_V.ICmp read _cmp_V write _cmp_V;
    property Item[Key: K]: V read GetItem write SetItem; default;
  end;

implementation

{ THashMap<K, V>.TPair }

constructor THashMap<K, V>.TPair.Create(newKey: K; newValue: V);
begin
  Key := newKey;
  Value := newValue;
end;

{ THashMap<K, V> }

constructor THashMap<K, V>.Create(newCapacity: integer);
var
  i: integer;
begin
  _cmp_K := TImpl_K.TCmp.Default;
  _cmp_V := TImpl_V.TCmp.Default;
  _capacity := newCapacity;
  _size := 0;
  SetLength(_data, _capacity);

  for i := 0 to high(_data) do
  begin
    _data[i] := TLinkedList_TPair.Create;
  end;
end;

function THashMap<K, V>.Add(Key: K; Value: V): TPtr_V;
var
  hashcode: integer;
  res: TPtr_V;
begin
  res := __getItem(Key);

  if res <> nil then
  begin
    SetItem(Key, Value);
    Result := res;
    Exit;
  end;

  hashcode := __hash(Key);
  _data[hashcode].AddLast(TPair.Create(Key, Value));
  _size := _size + 1;

  Result := nil;
end;

procedure THashMap<K, V>.AddAll(map: THashMap_K_V);
var
  e: TPair;
begin
  for e in map.Pairs do
  begin
    if not ContainsKey(e.Key) then
      Self.Add(e.Key, e.Value)
    else
      Self.SetItem(e.Key, e.Value);
  end;
end;

procedure THashMap<K, V>.Clear;
var
  i: integer;
begin
  for i := 0 to high(_data) do
  begin
    _data[i].Clear;
  end;

  _size := 0;
end;

function THashMap<K, V>.Clone: THashMap_K_V;
var
  res: THashMap_K_V;
  e: TPair;
begin
  res := THashMap_K_V.Create(_capacity);

  for e in Self.Pairs do
  begin
    res.Add(e.Key, e.Value);
  end;

  Result := res;
end;

function THashMap<K, V>.ContainsKey(Key: K): boolean;
var
  hashcode, i: integer;
begin
  hashcode := __hash(Key);

  for i := 0 to _data[hashcode].Count - 1 do
  begin
    if _cmp_K.Compare(Key, _data[hashcode].Items[i].Key) = 0 then
      Exit(true);
  end;

  Result := false;
end;

function THashMap<K, V>.ContainsValue(Value: V): boolean;
var
  i, j: integer;
begin
  for i := 0 to high(_data) do
  begin
    for j := 0 to _data[i].Count - 1 do
    begin
      if _cmp_V.Compare(Value, _data[i].Items[j].Value) = 0 then
        Exit(true);
    end;
  end;

  Result := false;
end;

function THashMap<K, V>.Count: integer;
begin
  Result := _size;
end;

destructor THashMap<K, V>.Destroy;
var
  i: integer;
begin
  for i := 0 to high(_data) do
  begin
    _data[i].Free;
  end;

  inherited Destroy;
end;

function THashMap<K, V>.GetItem(Key: K): V;
var
  res: TPtr_V;
begin
  res := __getItem(Key);

  if res = nil then
    raise Exception.Create('The hash-table does not contain this key');

  Result := res.Value;
end;

function THashMap<K, V>.IsEmpty: boolean;
begin
  Result := _size = 0;
end;

function THashMap<K, V>.Keys: TImpl_K.TArr;
var
  list: TLinkedList_K;
  i, j: integer;
begin
  list := TLinkedList_K.Create;
  try
    for i := 0 to high(_data) do
    begin
      for j := 0 to _data[i].Count - 1 do
      begin
        list.AddLast(_data[i].Items[j].Key);
      end;
    end;

    Result := list.ToArray;
  finally
    list.Free;
  end;
end;

function THashMap<K, V>.Pairs: TImpl_TPair.TArr;
var
  list: TLinkedList_TPair;
  i, j: integer;
begin
  list := TLinkedList_TPair.Create;
  try
    for i := 0 to high(_data) do
    begin
      for j := 0 to _data[i].Count - 1 do
      begin
        list.AddLast(_data[i].Items[j]);
      end;
    end;

    Result := list.ToArray;
  finally
    list.Free;
  end;
end;

function THashMap<K, V>.Remove(Key: K): TPtr_V;
var
  res: TPtr_V;
  hashcode, i: integer;
begin
  res := __getItem(Key);

  if res = nil then
    raise Exception.Create('The hash-table does not contain this key');

  hashcode := __hash(Key);

  for i := 0 to _data[hashcode].Count - 1 do
  begin
    if _cmp_K.Compare(Key, _data[hashcode].Items[i].Key) = 0 then
    begin
      _data[hashcode].Remove(i);
      _size := _size - 1;
      Break;
    end;
  end;

  Result := res;
end;

procedure THashMap<K, V>.SetItem(Key: K; Value: V);
var
  hashcode, i: integer;
begin
  hashcode := __hash(Key);

  for i := 0 to _data[hashcode].Count - 1 do
  begin
    if _cmp_K.Compare(Key, _data[hashcode].Items[i].Key) = 0 then
    begin
      _data[hashcode].Items[i].Value := Value;
      Break;
    end;
  end;
end;

function THashMap<K, V>.Values: TImpl_V.TArr;
var
  list: TLinkedList_V;
  i, j: integer;
begin
  list := TLinkedList_V.Create;
  try
    for i := 0 to high(_data) do
    begin
      for j := 0 to _data[i].Count - 1 do
      begin
        list.AddLast(_data[i].Items[j].Value);
      end;
    end;

    Result := list.ToArray;
  finally
    list.Free;
  end;
end;

function THashMap<K, V>.__getItem(Key: K): TPtr_V;
var
  hashcode, i: integer;
  Value: V;
  res: TPtr_V;
begin
  res := nil;
  hashcode := __hash(Key);

  for i := 0 to _data[hashcode].Count - 1 do
  begin
    if _cmp_K.Compare(Key, _data[hashcode].Items[i].Key) = 0 then
    begin
      Value := _data[hashcode].Items[i].Value;
      res := TPtr_V.Create(Value);
      Break;
    end;
  end;

  Result := res;
end;

function THashMap<K, V>.__hash(Key: K): integer;
var
  Value: TValue;
begin
  TValue.Make(@Key, TypeInfo(K), Value);
  Result := (Value.ToString.GetHashCode and $7FFFFFFF) mod _capacity;
end;

end.
