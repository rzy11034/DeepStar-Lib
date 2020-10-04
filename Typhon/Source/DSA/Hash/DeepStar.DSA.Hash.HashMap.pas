unit DeepStar.DSA.Hash.HashMap;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  {%H-}Rtti,
  DeepStar.DSA.Interfaces,
  DeepStar.DSA.Linear.LinkedList;

type
  generic THashMap<K, V> = class(TInterfacedObject, specialize IMap<K, V>)
  public type
    TPair = class
      Key: K;
      Value: V;
      constructor Create(newKey: K; newValue: V);
    end;

  private type
    TPtrValue = specialize TPtrValue<V>;
    TImpl_K = specialize TImpl<K>;
    TImpl_V = specialize TImpl<V>;
    TImpl_TPair = specialize TImpl<TPair>;
    TLinkedList_K = specialize TLinkedList<K>;
    TLinkedList_V = specialize TLinkedList<V>;
    TLinkedList_TPair = specialize TLinkedList<TPair>;

    THashMap_K_V = specialize THashMap<K, V>;

  private
    _data: array of TLinkedList_TPair;
    _capacity: integer;
    _size: integer;
    _cmp_K: TImpl_K.ICmp;
    _cmp_V: TImpl_V.ICmp;

    function __getItem(key: K): TPtrValue;
    function __hash(key: K): integer;

  public
    constructor Create(newCapacity: integer = 31);
    destructor Destroy; override;

    function Add(key: K; Value: V): TPtrValue;
    function Clone: THashMap_K_V;
    function ContainsKey(key: K): boolean;
    function ContainsValue(Value: V): boolean;
    function Count: integer;
    function GetItem(key: K): V;
    function IsEmpty: boolean;
    function Keys: TImpl_K.TArr;
    function Pairs: TImpl_TPair.TArr;
    function Remove(key: K): TPtrValue;
    function Values: TImpl_V.TArr;
    procedure AddAll(map: THashMap_K_V);
    procedure Clear;
    procedure SetItem(key: K; Value: V);

    property Comparer_K: TImpl_K.ICmp read _cmp_K write _cmp_K;
    property Comparer_V: TImpl_V.ICmp read _cmp_V write _cmp_V;
    property Item[key: K]: V read GetItem write SetItem; default;
  end;

implementation

{ THashMap.TPair }

constructor THashMap.TPair.Create(newKey: K; newValue: V);
begin
  Key := newKey;
  Value := newValue;
end;

{ THashMap }

constructor THashMap.Create(newCapacity: integer);
var
  i: integer;
begin
  _cmp_K := TImpl_K.TCmp.Default;
  _cmp_V := TImpl_V.TCmp.Default;
  _capacity := newCapacity;
  _size := 0;
  SetLength(_data, _capacity);

  for i := 0 to High(_data) do
  begin
    _data[i] := TLinkedList_TPair.Create;
  end;
end;

function THashMap.Add(key: K; Value: V): TPtrValue;
var
  hashcode: integer;
  res: TPtrValue;
begin
  res := __getItem(key);

  if res <> nil then
  begin
    SetItem(key, Value);
    Result := res;
    Exit;
  end;

  hashcode := __hash(key);
  _data[hashcode].AddLast(TPair.Create(key, Value));
  _size += 1;

  Result := nil;
end;

procedure THashMap.AddAll(map: THashMap_K_V);
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

procedure THashMap.Clear;
var
  i: integer;
begin
  for i := 0 to High(_data) do
  begin
    _data[i].Clear;
  end;

  _size := 0;
end;

function THashMap.Clone: THashMap_K_V;
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

function THashMap.ContainsKey(key: K): boolean;
var
  hashcode, i: integer;
begin
  hashcode := __hash(key);

  for i := 0 to _data[hashcode].Count - 1 do
  begin
    if _cmp_K.Compare(key, _data[hashcode].Items[i].Key) = 0 then
      Exit(true);
  end;

  Result := false;
end;

function THashMap.ContainsValue(Value: V): boolean;
var
  i, j: integer;
begin
  for i := 0 to High(_data) do
  begin
    for j := 0 to _data[i].Count - 1 do
    begin
      if _cmp_V.Compare(Value, _data[i].Items[j].Value) = 0 then
        Exit(true);
    end;
  end;

  Result := false;
end;

function THashMap.Count: integer;
begin
  Result := _size;
end;

destructor THashMap.Destroy;
var
  i: integer;
begin
  for i := 0 to High(_data) do
  begin
    _data[i].Free;
  end;

  inherited Destroy;
end;

function THashMap.GetItem(key: K): V;
var
  res: TPtrValue;
begin
  res := __getItem(Key);

  if res = nil then
    raise Exception.Create('The hash-table does not contain this key');

  Result := res.Value;
end;

function THashMap.IsEmpty: boolean;
begin
  Result := _size = 0;
end;

function THashMap.Keys: TImpl_K.TArr;
var
  list: TLinkedList_K;
  i, j: integer;
begin
  list := TLinkedList_K.Create;
  try
    for i := 0 to High(_data) do
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

function THashMap.Pairs: TImpl_TPair.TArr;
var
  list: TLinkedList_TPair;
  i, j: integer;
begin
  list := TLinkedList_TPair.Create;
  try
    for i := 0 to High(_data) do
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

function THashMap.Remove(key: K): TPtrValue;
var
  res: TPtrValue;
  hashcode, i: integer;
begin
  res := __getItem(Key);

  if res = nil then
    raise Exception.Create('The hash-table does not contain this key');

  hashcode := __hash(key);

  for i := 0 to _data[hashcode].Count - 1 do
  begin
    if _cmp_K.Compare(key, _data[hashcode].Items[i].Key) = 0 then
    begin
      _data[hashcode].Remove(i);
      _size -= 1;
      Break;
    end;
  end;

  Result := res;
end;

procedure THashMap.SetItem(key: K; Value: V);
var
  hashcode, i: integer;
begin
  hashcode := __hash(key);

  for i := 0 to _data[hashcode].Count - 1 do
  begin
    if _cmp_K.Compare(key, _data[hashcode].Items[i].Key) = 0 then
    begin
      _data[hashcode].Items[i].Value := Value;
      Break;
    end;
  end;
end;

function THashMap.Values: TImpl_V.TArr;
var
  list: TLinkedList_V;
  i, j: integer;
begin
  list := TLinkedList_V.Create;
  try
    for i := 0 to High(_data) do
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

function THashMap.__getItem(key: K): TPtrValue;
var
  hashcode, i: integer;
  Value: V;
  res: TPtrValue;
begin
  res := nil;
  hashcode := __hash(key);

  for i := 0 to _data[hashcode].Count - 1 do
  begin
    if _cmp_K.Compare(key, _data[hashcode].Items[i].Key) = 0 then
    begin
      Value := _data[hashcode].Items[i].Value;
      res := TPtrValue.Create(Value);
      Break;
    end;
  end;

  Result := res;
end;

function THashMap.__hash(key: K): integer;
var
  Value: TValue;
begin
  TValue.Make(@key, TypeInfo(K), Value);
  Result := (Value.ToString.GetHashCode and $7FFFFFFF) mod _capacity;
end;

end.
