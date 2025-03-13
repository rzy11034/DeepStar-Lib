
{ TQueue }

constructor TQueue.Create;
begin
  fData := TDoubleLinkedList.Create;
end;

function TQueue.Count: integer;
begin
  Result := fData.Count;
end;

function TQueue.DeQueue: T;
begin
  Result := fData.RemoveFirst;
end;

destructor TQueue.Destroy;
begin
  fData.Free;
  inherited Destroy;
end;

procedure TQueue.EnQueue(e: T);
begin
  fData.AddLast(e);
end;

function TQueue.IsEmpty: boolean;
begin
  Result := fData.IsEmpty;
end;

function TQueue.Peek: T;
begin
  Result := fData.GetFirst;
end;

