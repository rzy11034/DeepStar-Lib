unit DeepStar.DSA.Strings.Test.RabinKarp;

interface

uses
  DeepStar.Utils,
  DeepStar.DSA.Strings.RabinKarp,
  DeepStar.Utils.UString;

procedure Main;

implementation

procedure Main;
var
  s, p: UString;
begin
  s := 'ABABABA';
  p := 'ABA';

  TArrayUtils_int.Print(TRabinKarp.Match_Simplicity(s, p));
  TArrayUtils_int.Print(TRabinKarp.Match(s, p));
end;

end.
