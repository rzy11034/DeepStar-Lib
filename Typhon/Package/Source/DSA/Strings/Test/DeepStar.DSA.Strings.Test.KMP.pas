unit DeepStar.DSA.Strings.Test.KMP;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils, DeepStar.UString,
  DeepStar.DSA.Strings.KMP;

procedure Main;

implementation

procedure Main;
var
  s, p: UString;
begin
  s := 'ABABABA';
  p := 'ABA';

  TArrayUtils_int.Print(TKMP.IndexOf_Simplicity(s, p));
  TArrayUtils_int.Print(TKMP.IndexOf(s, p));
end;

end.
