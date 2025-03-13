

{ TArrayList }

procedure TArrayList.Add(index: integer; e: T);
var
  i: integer;
begin
  if (index < 0) or (index > fSize) then
    raise Exception.Create('Add failed. Require index >= 0 and index <= Size.');

  if (fSize = Length(fData)) then
    ResizeIncreases;

  for i := fSize - 1 downto index do
    fData[i + 1] := fData[i];

  fData[index] := e;
  fSize += 1;
end;

procedure TArrayList.AddFirst(e: T);
begin
  Add(0, e);
end;

procedure TArrayList.AddLast(e: T);
begin
  Add(fSize, e);
end;

procedure TArrayList.AddRange(const arr: array of T);
begin
  AddRange(arr, 0, Length(arr));
end;

procedure TArrayList.AddRange(const arr: array of T; aIndex, aCount: integer);
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

procedure TArrayList.Clear;
begin
  fSize := 0;
  fData := nil;
  SetLength(fData, 10);
end;

function TArrayList.Contains(e: T): boolean;
var
  i: integer;
begin
  for i := 0 to fSize - 1 do
  begin
    if fCmp.Compare(fData[i], e) = 0 then
      Exit(true);
  end;

  Result := false;
end;

destructor TArrayList.Destroy;
begin
  inherited Destroy;
end;

constructor TArrayList.Create(capacity: integer);
begin
  if capacity < 4 then capacity := 4;


  SetLength(fData, capacity);
  fCmp := TImpl.TCmp.Default;

  fSize := 0;
  fCapacity := capacity;
end;

constructor TArrayList.Create(const arr: TArr);
var
  i: integer;
begin
  SetLength(fData, Length(arr));

  for i := 0 to Length(arr) - 1 do
    fData[i] := arr[i];

  fSize := Length(arr);
  fCapacity := fSize;
end;

constructor TArrayList.Create(cmp: TImpl.ICmp);
begin
  Self.Create;
  fCmp := cmp;
end;

constructor TArrayList.Create(comparisonFunc: TImpl.TComparisonFuncs);
begin
  Self.Create;
  fCmp := TImpl.TCmp.Construct(ComparisonFunc);
end;

constructor TArrayList.Create(onComparison: TImpl.TOnComparisons);
begin
  Self.Create;
  fCmp := TImpl.TCmp.Construct(OnComparison);
end;

function TArrayList.IndexOf(e: T): integer;
var
  i: integer;
begin
  for i := 0 to fSize - 1 do
  begin
    if fCmp.Compare(fData[i], e) = 0 then
      Exit(i);
  end;

  Result := -1;
end;

function TArrayList.GetItem(index: integer): T;
begin
  if (index < 0) or (index >= fSize) then
    raise Exception.Create('Get failed. Index is illegal.');

  Result := fData[index];
end;

function TArrayList.GetCapacity: integer;
begin
  Result := Length(Self.fData);
end;

function TArrayList.GetFirst: T;
begin
  Result := GetItem(0);
end;

function TArrayList.GetLast: T;
begin
  Result := GetItem(fSize - 1);
end;

function TArrayList.GetSize: integer;
begin
  Result := Self.fSize;
end;

function TArrayList.IsEmpty: boolean;
begin
  Result := Self.fSize = 0;
end;

function TArrayList.Remove(index: integer): T;
var
  i: integer;
  res: T;
begin
  if (index < 0) or (index >= fSize) then
    raise Exception.Create('Remove failed. Index is illegal.');

  res := fData[index];

  for i := index + 1 to fSize - 1 do
    fData[i - 1] := fData[i];

  Dec(Self.fSize);

  ResizeDecreases;

  Result := res;
end;

procedure TArrayList.RemoveElement(e: T);
var
  index, i: integer;
begin
  for i := 0 to fSize - 1 do
  begin
    index := IndexOf(e);

    if index <> -1 then
      Remove(index);
  end;
end;

function TArrayList.RemoveFirst: T;
begin
  Result := Remove(0);
end;

function TArrayList.RemoveLast: T;
begin
  Result := Remove(fSize - 1);
end;

procedure TArrayList.Reverse;
var
  l, r: integer;
begin
  l := 0;
  r := fSize - 1;

  while l < r do
  begin
    Swap(fData[l], fData[r]);
    l += 1;
    r -= 1;
  end;
end;

procedure TArrayList.SetItem(index: integer; e: T);
begin
  if (index < 0) or (index >= fSize) then
    raise Exception.Create('Set failed. Require index >= 0 and index < Size.');

  fData[index] := e;
end;

procedure TArrayList.Sort;
begin
  QuickSort(0, fSize - 1);
end;

function TArrayList.ToArray: TArr;
var
  i: integer;
  arr: TArr;
begin
  SetLength(arr, fSize);

  for i := 0 to fSize - 1 do
    arr[i] := fData[i];

  Result := arr;
end;

function TArrayList.ToString: string;
var
  res: TStringBuilder;
  i: integer;
  Value: TValue;
begin
  res := TStringBuilder.Create;
  try
    res.AppendFormat('Array: Size = %d, capacity = %d',
      [Self.fSize, Length(Self.fData)]);
    res.AppendLine;
    res.Append('  [');

    for i := 0 to fSize - 1 do
    begin
      TValue.Make(@fData[i], TypeInfo(T), Value);

      if not (Value.IsObject) then
        res.Append(Value.ToString)
      else
        res.Append(Value.AsObject.ToString);

      if i <> fSize - 1 then
        res.Append(', ');
    end;

    res.Append(']');
    Result := res.ToString;

  finally
    res.Free;
  end;
end;

function TArrayList.GetItemPtr(index: integer): PT;
begin
  if (index < 0) or (index >= fSize) then
    raise Exception.Create('Set failed. Require index >= 0 and index < Size.');

  Result := @fData[index];
end;

procedure TArrayList.QuickSort(l, r: integer);
var
  i, j: integer;
  p, q: T;
begin
  if ((r - l) <= 0) or (Length(fData) = 0) then
    Exit;

  repeat
    i := l;
    j := r;
    p := fData[l + (r - l) shr 1];
    repeat
      while fCmp.Compare(fData[i], p) < 0 do
        Inc(i);
      while fCmp.Compare(fData[j], p) > 0 do
        Dec(j);
      if i <= j then
      begin
        if i <> j then
        begin
          q := fData[i];
          fData[i] := fData[j];
          fData[j] := q;
        end;
        Inc(i);
        Dec(j);
      end;
    until i > j;
    // sort the smaller range recursively
    // sort the bigger range via the loop
    // Reasons: memory usage is O(log(n)) instead of O(n) and loop is faster than recursion
    if j - l < r - i then
    begin
      if l < j then
        QuickSort(l, j);
      l := i;
    end
    else
    begin
      if i < r then
        QuickSort(i, r);
      r := j;
    end;
  until l >= r;
end;

procedure TArrayList.ResizeDecreases;
begin
  if fCapacity >= 20 then
  begin
    if (fSize = Length(Self.fData) div 4) and (Length(Self.fData) div 2 <> 0) then
    begin
      SetLength(Self.fData, Length(Self.fData) div 4);
      fCapacity := fSize;
    end;
  end
  else
    Exit;
end;

procedure TArrayList.ResizeIncreases;
  function NewCapacity: SizeUInt;
  const
    // if size is small, multiply by 2;
    // if size bigger but <256M, inc by 1/8*size;
    // otherwise inc by 1/16*size
    cSizeSmall = 1 * 1024 * 1024;
    cSizeBig = 256 * 1024 * 1024;
  var
    dataSize: SizeUInt;
    res: SizeUInt;
  begin
    dataSize := fCapacity * SizeOf(T);

    if fCapacity = 0 then
      res := 4
    else if dataSize < cSizeSmall then
      res := fCapacity * 2
    else if dataSize < cSizeBig then
      res := fCapacity + fCapacity div 8
    else
      res := fCapacity + fCapacity div 16;

    Result := res;
  end;

begin
  fCapacity := NewCapacity;
  SetLength(Self.fData, fCapacity);
end;

procedure TArrayList.SetComparer(const newComparer: TImpl.ICmp);
begin
  fCmp := newComparer;
end;

procedure TArrayList.Swap(var a, b: T);
var
  temp: T;
begin
  temp := a;
  a := b;
  b := temp;
end;

