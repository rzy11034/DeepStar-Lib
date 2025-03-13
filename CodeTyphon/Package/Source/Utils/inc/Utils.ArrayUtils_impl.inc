class function TArrayUtils.BinarySearch(const arr: TArr_T; const e: T): integer;
var
  ret: SizeInt;
begin
  Result := -1;

  if TArrayHelper_T.BinarySearch(arr, e, ret) then
    Result := ret;
end;

class function TArrayUtils.BinarySearch(const arr: TArr_T; const e: T;
  const cmp: ICmp_T): integer;
var
  ret: SizeInt;
begin
  Result := -1;

  if TArrayHelper_T.BinarySearch(arr, e, ret, cmp) then
    Result := ret;
end;

class function TArrayUtils.CopyArray(const arr: TArr_T): TArr_T;
begin
  Result := System.Copy(arr);
end;

class function TArrayUtils.CopyArray2D(const arr2D: TArr2D_T): TArr2D_T;
var
  i: integer;
  res: TArr2D_T;
begin
  SetLength(res, Length(arr2D));

  for i := 0 to Length(arr2D) - 1 do
    res[i] := Copy(arr2D[i]);

  Result := res;
end;

class procedure TArrayUtils.FillArray(var arr: TArr_T; e: T);
var
  i: integer;
begin
  for i := 0 to High(arr) do
    arr[i] := e;
end;

class function TArrayUtils.IndexOf(const arr: TArr_T; e: T): integer;
var
  i: integer;
  cmp: ICmp_T;
begin
  Result := -1;
  cmp := TCmp_T.Default;

  for i := 0 to High(arr) do
  begin
    if cmp.Compare(arr[i], e) = 0 then
      Result := i;
  end;
end;

class function TArrayUtils.MemorySize(const dynArray: TArr_T): integer;
begin
  Result := SizeOf(dynArray[0]) * Length(dynArray);
end;

class procedure TArrayUtils.Print(arr: TArr_T);
var
  i: integer;
  Value: TValue;
begin
  if arr = nil then
  begin
    WriteLn('[]');
    Exit;
  end;

  Write('[');
  for i := 0 to High(arr) do
  begin
    TValue.Make(@arr[i], TypeInfo(T), Value);
    if Value.Kind <> tkFloat then
      Write(Value.ToString)
    else
      Write(Value.AsExtended.ToString);

    if i <> High(arr) then
      Write(', ');
  end;
  Write(']'#10);
end;

class procedure TArrayUtils.Print2D(arr: TArr2D_T; formated: boolean);
var
  i, j: integer;
  Value: TValue;
  tmp: T;
begin
  if arr = nil then
  begin
    WriteLn('[[]]');
    Exit;
  end;

  case formated of
    true:
    begin
      for i := 0 to High(arr) do
      begin
        Write('[');
        for j := 0 to High(arr[i]) do
        begin
          tmp := arr[i, j];
          TValue.Make(@tmp, System.TypeInfo(T), Value);
          if Value.Kind <> tkFloat then
            WriteF('%6s', [Value.ToString])
          else
            WriteF('%6s', [Value.AsExtended.ToString]);

          if j <> High(arr[i]) then
            Write(', ');
        end;

        if i <> High(arr) then
          WriteLn('], ')
        else
          WriteLn(']');
      end;
    end;

    false:
    begin
      for i := 0 to High(arr) do
      begin
        Write('[');
        for j := 0 to High(arr[i]) do
        begin
          tmp := arr[i, j];
          TValue.Make(@tmp, TypeInfo(T), Value);
          if Value.Kind <> tkFloat then
            Write(Value.ToString)
          else
            Write(Value.AsExtended.ToString);

          if j <> High(arr[i]) then
            Write(', ');
        end;

        if i <> High(arr) then
          Write('], ')
        else
          Write(']');
      end;
    end;
  end;
end;

class procedure TArrayUtils.Print3D(arr: TArr3D_T);
var
  i, j, k: integer;
  Value: TValue;
  tmp: T;
begin
  if arr = nil then
  begin
    WriteLn('[]');
    Exit;
  end;

  for i := 0 to High(arr) do
  begin
    Write('[');
    for j := 0 to High(arr[i]) do
    begin
      Write('(');
      for k := 0 to High(arr[i, j]) do
      begin
        tmp := arr[i, j, k];
        TValue.Make(@tmp, TypeInfo(T), Value);
        if Value.Kind <> tkFloat then
          Write(Value.ToString)
        else
          Write(Value.AsExtended.ToString);

        if k <> High(arr[i, j]) then
          Write(',');
      end;
      Write(')');

      if j <> High(arr[i]) then
        Write(', ');
    end;
    Write(']'#10);
  end;
end;

class procedure TArrayUtils.Reverse(var arr: TArr_T);
var
  l, r: integer;
begin
  l := 0;
  r := High(arr);

  while l < r do
  begin
    specialize Swap<T>(arr[l], arr[r]);

    l += 1;
    r -= 1;
  end;
end;

class procedure TArrayUtils.SetLengthAndFill(var arr: TArr2D_T; n, m: integer);
var
  i: integer;
begin
  SetLength(arr, n, m);
  for i := 0 to High(arr) do
    FillArray(arr[i], Default(T));
end;

class procedure TArrayUtils.SetLengthAndFill(var arr: TArr2D_T; n, m: integer; f: T);
var
  i: integer;
begin
  SetLength(arr, n, m);
  for i := 0 to High(arr) do
    FillArray(arr[i], f);
end;

class procedure TArrayUtils.SetLengthAndFill(var arr: TArr_T; n: integer);
begin
  SetLength(arr, n);
  FillArray(arr, Default(T));
end;

class procedure TArrayUtils.SetLengthAndFill(var arr: TArr_T; n: integer; f: T);
begin
  SetLength(arr, n);
  FillArray(arr, f);
end;

class procedure TArrayUtils.Sort(var arr: TArr_T);
begin
  TArrayHelper_T.Sort(arr);
end;

class procedure TArrayUtils.Sort(var arr: TArr_T; index, Count: integer);
begin
  TArrayHelper_T.Sort(arr, TCmp_T.Default, index, Count);
end;

class procedure TArrayUtils.Sort(var arr: TArr_T; index, Count: integer; cmp: ICmp_T);
begin
  TArrayHelper_T.Sort(arr, cmp, index, Count);
end;

class procedure TArrayUtils.Sort(var arr: TArr_T; index, Count: integer; cmp: TComparisonFunc_T);
begin
  TArrayHelper_T.Sort(arr, TCmp_T.Construct(cmp), index, Count);
end;

class procedure TArrayUtils.Sort(var arr: TArr_T; index, Count: integer; cmp: TOnComparison_T);
begin
  TArrayHelper_T.Sort(arr, TCmp_T.Construct(cmp), index, Count);
end;

class procedure TArrayUtils.Sort(var arr: TArr_T; cmp: ICmp_T);
begin
  TArrayHelper_T.Sort(arr, cmp);
end;

class procedure TArrayUtils.Sort(var arr: TArr_T; cmp: TComparisonFunc_T);
begin
  TArrayHelper_T.Sort(arr, TCmp_T.Construct(cmp));
end;

class procedure TArrayUtils.Sort(var arr: TArr_T; cmp: TOnComparison_T);
begin
  TArrayHelper_T.Sort(arr, TCmp_T.Construct(cmp));
end;

