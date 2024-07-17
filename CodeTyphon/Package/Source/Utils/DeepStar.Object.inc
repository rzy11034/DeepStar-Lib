  {%MainUint DeepStar.Utils.pas}

  {$IfDef InterfacePart}

type
  PObj = ^TObj;
  TObj = object
  private

  public
    constructor Init();
    destructor Done; virtual;

  end;

  {$Else}

  { TObj }

constructor TObj.Init();
begin
  inherited;
end;

destructor TObj.Done;
begin
  inherited;
end;

{$EndIf}
