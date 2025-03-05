  {%MainUint DeepStar.Utils.pas}

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
