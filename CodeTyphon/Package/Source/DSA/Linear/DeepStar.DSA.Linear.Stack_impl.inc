
{ TStack }

constructor TStack.Create;
begin
  fData := TDoubleLinkedList.Create;
end;

function TStack.Count: integer;
begin
  Result := fData.Count;
end;

destructor TStack.Destroy;
begin
  fData.Free;
  inherited Destroy;
end;

function TStack.IsEmpty: boolean;
begin
  Result := fData.IsEmpty;
end;

function TStack.Peek: T;
begin
  Result := fData.GetLast;
end;

function TStack.Pop: T;
begin
  Result := fData.RemoveLast;
end;

procedure TStack.Push(e: T);
begin
  fData.AddLast(e);
end;

