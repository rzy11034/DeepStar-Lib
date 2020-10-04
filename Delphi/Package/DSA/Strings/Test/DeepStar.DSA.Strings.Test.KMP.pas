unit DeepStar.DSA.Strings.Test.KMP;

interface

uses
  DeepStar.Utils,
  DeepStar.DSA.Strings.KMP,
  DeepStar.Utils.UString;

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
