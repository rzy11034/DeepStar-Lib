  {%MainUint DeepStar.Utils.pas}

  {$IfDef InterfacePart}

type
  PObj_Base = ^TObj_Base;
  TObj_Base = object
  private

  public
    constructor Init();
    destructor Done; virtual;

  end;

// 释放一个 object 对象，并设置为 nil
procedure FreeObjAndNil(var aObj: PObj_Base);

  {$Else}

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

{$EndIf}
