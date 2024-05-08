unit DeepStar.DSA.Strings.Test.RabinKarp;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils,
  DeepStar.DSA.Strings.RabinKarp;

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

