unit ArrayListTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testutils,
  testregistry,
  DeepStar.DSA.Linear.ArrayList,
  DeepStar.Utils,
  DeepStar.UString;

type

  TArrayListTestCase = class(TTestCase)
  private
    _list: TList_int;

  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_Add;
  end;

implementation

procedure TArrayListTestCase.Test_Add;
var
  s: UString;
begin
  _list.AddLast(1);
  s := '1';
  _list.AddLast(s.ToInteger);
  Fail('编写你自己的测试');
end;

procedure TArrayListTestCase.SetUp;
begin
  _list := TList_int.Create;
end;

procedure TArrayListTestCase.TearDown;
begin
  FreeAndNil(_list);
end;

initialization

  RegisterTest(TArrayListTestCase);
end.
