
{ TLinkedList.TNode }

constructor TLinkedList.TNode.Create(newE: T; newNext: TNode);
begin
  E := newE;
  Next := newNext;
end;

constructor TLinkedList.TNode.Create;
begin
  Self.Create(Default(T));
end;

destructor TLinkedList.TNode.Destroy;
begin
  inherited Destroy;
end;

{ TLinkedList }

constructor TLinkedList.Create(const arr: TArr);
begin
  Self.Create;
  Self.AddRange(arr);
end;

constructor TLinkedList.Create(cmp: TImpl.ICmp);
begin
  fCmp := cmp;
end;

constructor TLinkedList.Create(comparisonFunc: TImpl.TComparisonFuncs);
begin
  Self.Create;
  fCmp := TCmp.Construct(comparisonFunc);
end;

constructor TLinkedList.Create(onComparison: TImpl.TOnComparisons);
begin
  Self.Create;
  fCmp := TCmp.Construct(onComparison);
end;

constructor TLinkedList.Create;
begin
  fDummyHead := TNode.Create;
  fSize := 0;
  fCmp := TImpl.TCmp.Default;
end;

procedure TLinkedList.Add(index: integer; e: T);
var
  prev: TNode;
  i: integer;
begin
  if (index < 0) or (index > fSize) then
    raise Exception.Create('Add failed. Index is Illegal.');

  prev := fDummyHead;
  for i := 0 to index - 1 do
    prev := prev.Next;

  prev.Next := TNode.Create(e, prev.Next);
  fSize += 1;
end;

procedure TLinkedList.AddFirst(e: T);
begin
  Self.Add(0, e);
end;

procedure TLinkedList.AddLast(e: T);
begin
  Self.Add(fSize, e);
end;

procedure TLinkedList.AddRange(const arr: array of T);
var
  i: integer;
  prev, tmp: TNode;
begin
  prev := fDummyHead;

  while prev.Next <> nil do
    prev := prev.Next;

  for i := 0 to High(arr) do
  begin
    tmp := TNode.Create(arr[i]);
    prev.Next := tmp;
    prev := tmp;
    fSize += 1;
  end;
end;

procedure TLinkedList.AddRange(const arr: array of T; aIndex, aCount: integer);
var
  i: integer;
  prev, tmp: TNode;
begin
  if (aIndex < 0) or (aIndex + aCount > Length(arr)) then
    raise Exception.Create('Add failed.');

  prev := fDummyHead;

  while prev.Next <> nil do
    prev := prev.Next;

  for i := aIndex to aCount - 1 do
  begin
    tmp := TNode.Create(arr[i]);
    prev.Next := tmp;
    prev := tmp;
    fSize += 1;
  end;
end;

procedure TLinkedList.Clear;
var
  cur, del: TNode;
begin
  cur := fDummyHead.Next;

  while cur <> nil do
  begin
    del := cur;
    cur := cur.Next;

    FreeAndNil(del);
  end;

  fDummyHead.Next := nil;
  fSize := 0;
end;

function TLinkedList.Contains(e: T): boolean;
var
  cur: TNode;
begin
  cur := fDummyHead.Next;

  while cur <> nil do
  begin
    if fCmp.Compare(cur.E, e) = 0 then
      Exit(true);

    cur := cur.Next;
  end;

  Result := false;
end;

destructor TLinkedList.Destroy;
begin
  Self.Clear;
  FreeAndNil(fDummyHead);

  inherited Destroy;
end;

function TLinkedList.GetFirst: T;
begin
  Result := GetItem(0);
end;

function TLinkedList.GetItem(index: integer): T;
var
  cur: TNode;
  i: integer;
begin
  if (index < 0) or (index >= fSize) then
    raise Exception.Create('GetItem failed. Index is Illegal.');

  cur := fDummyHead.Next;

  for i := 0 to index - 1 do
    cur := cur.Next;

  Result := cur.E;
end;

function TLinkedList.GetLast: T;
begin
  Result := GetItem(fSize - 1);
end;

function TLinkedList.GetSize: integer;
begin
  Result := fSize;
end;

function TLinkedList.IndexOf(e: T): integer;
var
  i: integer;
  cur: TNode;
begin
  i := 0;
  cur := fDummyHead.Next;

  while cur <> nil do
  begin
    if fCmp.Compare(cur.E, e) = 0 then
      Exit(i);

    cur := cur.Next;
    i += 1;
  end;

  Result := -1;
end;

function TLinkedList.IsEmpty: boolean;
begin
  Result := fSize <> 0;
end;

function TLinkedList.Remove(index: integer): T;
var
  prev, del: TNode;
  i: integer;
begin
  if (index < 0) or (index >= fSize) then
    raise Exception.Create('Remove failed. Index is Illegal.');

  prev := fDummyHead;
  for i := 0 to index - 1 do
    prev := prev.Next;

  del := prev.Next;
  prev.Next := del.Next;
  fSize -= 1;
  Result := del.E;
  FreeAndNil(del);
end;

procedure TLinkedList.RemoveElement(e: T);
var
  prev, del: TNode;
begin
  prev := fDummyHead;

  while prev.Next <> nil do
  begin
    del := prev.Next;

    if fCmp.Compare(del.E, e) = 0 then
    begin
      prev.Next := del.Next;
      fSize -= 1;
      FreeAndNil(del);
    end
    else
    begin
      prev := prev.Next;
    end;
  end;
end;

function TLinkedList.RemoveFirst: T;
begin
  Result := Remove(0);
end;

function TLinkedList.RemoveLast: T;
begin
  Result := Remove(Count - 1);
end;

procedure TLinkedList.Reverse;
var
  pre, cur, Next: TNode;
begin
  pre := nil;
  cur := fDummyHead.Next;

  while cur <> nil do
  begin
    Next := cur.Next;

    cur.Next := pre;
    pre := cur;
    cur := Next;
  end;

  fDummyHead.Next := pre;
end;

procedure TLinkedList.SetItem(index: integer; e: T);
var
  cur: TNode;
  i: integer;
begin
  if (index < 0) or (index >= fSize) then
    raise Exception.Create('SetItem failed. Index is Illegal.');

  cur := fDummyHead.Next;
  for i := 0 to index - 1 do
    cur := cur.Next;

  cur.E := e;
end;

function TLinkedList.ToArray: TImpl.TArr;
var
  res: TImpl.TArr;
  cur: TNode;
  i: integer;
begin
  SetLength(res, Count);
  cur := fDummyHead.Next;

  for i := 0 to High(res) do
  begin
    res[i] := cur.E;
    cur := cur.Next;
  end;

  Result := res;
end;

function TLinkedList.ToString: string;
var
  sb: TStringBuilder;
  cur: TNode;
  e: T;
  Value: TValue;
begin
  sb := TStringBuilder.Create;
  try
    cur := fDummyHead.Next;

    while cur <> nil do
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

procedure TLinkedList.fSetComparer(const newComparer: ICmp);
begin
  fCmp := newComparer;
end;

