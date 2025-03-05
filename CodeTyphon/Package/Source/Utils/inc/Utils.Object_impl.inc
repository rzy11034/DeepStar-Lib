
procedure FreeObjAndNil(var aObj: PObj_Base);
var
  temp: PObj_Base;
begin
  temp := aObj;
  aObj := nil;
  Dispose(temp, Done);
end;

  { TObj_Base }

constructor TObj_Base.Init();
begin
  inherited;
end;

destructor TObj_Base.Done;
begin
  inherited;
end;
