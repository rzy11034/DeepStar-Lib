
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
  fCmp := cmp;
end;

constructor TDoubleLinkedList.Create(comparisonFunc: TImpl.TComparisonFuncs);
begin
  Self.Create;
  fCmp := TCmp.Construct(comparisonFunc);
end;

constructor TDoubleLinkedList.Create(onComparison: TImpl.TOnComparisons);
begin
  Self.Create;
  fCmp := TCmp.Construct(onComparison);
end;

constructor TDoubleLinkedList.Create;
begin
  fDummyhead := TNode.Create;
  FdummyTail := TNode.Create;
  fDummyhead.Next := FdummyTail;
  FdummyTail.Prev := fDummyhead;
  fSize := 0;
  fCmp := TImpl.TCmp.Default;
end;

procedure TDoubleLinkedList.Add(index: integer; e: T);
var
  cur, tmp: TNode;
  mid, i: integer;
begin
  if (index < 0) or (index > fSize) then
    raise Exception.Create('Add failed. Index is Illegal.');

  if IsEmpty then
  begin
    tmp := TNode.Create(e);
    fDummyhead.Next := tmp;
    FdummyTail.Prev := tmp;
    tmp.Prev := fDummyhead;
    tmp.Next := FdummyTail;
    fSize += 1;
    Exit;
  end;

  mid := fSize div 2;

  if (index >= mid) and (fSize > 3) then
  begin
    cur := FdummyTail.Prev;
    for i := fSize - 1 downto index + 1 do
      cur := cur.Prev;

    tmp := TNode.Create(e);
    tmp.prev := cur;
    tmp.Next := cur.Next;
    cur.Next.Prev := tmp;
    cur.Next := tmp;
  end
  else
  begin
    cur := fDummyhead.Next;
    for i := 0 to index - 1 do
      cur := cur.Next;

    tmp := TNode.Create(e);
    tmp.prev := cur.Prev;
    tmp.Next := cur;
    cur.Prev.Next := tmp;
    cur.Prev := tmp;
  end;

  fSize += 1;
end;

procedure TDoubleLinkedList.AddFirst(e: T);
begin
  Self.Add(0, e);
end;

procedure TDoubleLinkedList.AddLast(e: T);
begin
  Self.Add(fSize, e);
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
  cur := fDummyhead.Next;

  while cur <> FdummyTail do
  begin
    if fCmp.Compare(cur.E, e) = 0 then
      Exit(true);

    cur := cur.Next;
  end;

  Result := false;
end;

destructor TDoubleLinkedList.Destroy;
begin
  Self.Clear;
  FreeAndNil(FdummyTail);
  FreeAndNil(fDummyhead);
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
  if (index < 0) or (index >= fSize) then
    raise Exception.Create('GetItem failed. Index is Illegal.');

  mid := fSize div 2;

  if index >= mid then
  begin
    cur := FdummyTail.Prev;
    for i := fSize - 1 downto index + 1 do
      cur := cur.Prev;
  end
  else
  begin
    cur := fDummyhead.Next;
    for i := 0 to index - 1 do
      cur := cur.Next;
  end;

  Result := cur.E;
end;

function TDoubleLinkedList.GetLast: T;
begin
  Result := GetItem(fSize - 1);
end;

function TDoubleLinkedList.GetSize: integer;
begin
  Result := fSize;
end;

function TDoubleLinkedList.IndexOf(e: T): integer;
var
  i: integer;
  cur: TNode;
begin
  i := 0;
  cur := fDummyhead.Next;

  while cur <> nil do
  begin
    if fCmp.Compare(cur.E, e) = 0 then
      Exit(i);

    cur := cur.Next;
    i += 1;
  end;

  Result := -1;
end;

function TDoubleLinkedList.IsEmpty: boolean;
begin
  Result := fSize = 0;
end;

function TDoubleLinkedList.Remove(index: integer): T;
var
  del: TNode;
  i, mid: integer;
  res: T;
begin
  if (index < 0) or (index >= fSize) then
    raise Exception.Create('Remove failed. Index is Illegal.');

  mid := fSize div 2;

  if index >= mid then
  begin
    del := FdummyTail.Prev;
    for i := fSize - 1 downto index + 1 do
      del := del.Prev;
  end
  else
  begin
    del := fDummyhead.Next;
    for i := 0 to index - 1 do
      del := del.Next;
  end;

  del.Prev.Next := del.Next;
  del.Next.Prev := del.Prev;
  res := del.E;
  FreeAndNil(del);
  fSize -= 1;

  Result := res;
end;

procedure TDoubleLinkedList.RemoveElement(e: T);
var
  cur, del: TNode;
begin
  cur := fDummyhead.Next;

  while cur <> FdummyTail do
  begin
    if fCmp.Compare(cur.E, e) = 0 then
    begin
      del := cur;
      cur.Prev.Next := del.Next;
      cur.Next.Prev := del.Prev;
      cur := cur.Next;
      FreeAndNil(del);
      fSize -= 1;
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
  Result := Remove(fSize - 1);
end;

procedure TDoubleLinkedList.Reverse;
var
  head, tail: TNode;
  l, r: integer;
begin
  if fSize <= 1 then Exit;

  l := 0;
  r := fSize - 1;
  head := fDummyhead.Next;
  tail := FdummyTail.Prev;

  while l < r do
  begin
    Swap(head.E, tail.E);
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
  if (index < 0) or (index >= fSize) then
    raise Exception.Create('SetItem failed. Index is Illegal.');

  mid := fSize div 2;

  if index >= mid then
  begin
    cur := FdummyTail.Prev;
    for i := fSize - 1 downto index + 1 do
      cur := cur.Prev;
  end
  else
  begin
    cur := fDummyhead.Next;
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
  cur := fDummyhead.Next;

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
    cur := fDummyhead.Next;

    while cur <> FdummyTail do
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

procedure TDoubleLinkedList.SetComparer(const newComparer: TImpl.ICmp);
begin
  fCmp := newComparer;
end;

procedure TDoubleLinkedList.Swap(var a, b: T);
var
  temp: T;
begin
  temp := a;
  a := b;
  b := temp;
end;

