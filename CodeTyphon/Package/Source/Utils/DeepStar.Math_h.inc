  {%MainUint DeepStar.Utils.pas}   

type
  TMath = class(TObject)
  private
    class function __CharToNum(c: UChar): integer;
    class function __NumToChar(n: integer): char;

  public
    // 转换任意进制字符串到十进制数
    class function AnyToDec(numStr: string; NumberSystem: integer): integer;
    // 转换十进制数到任意进制字符串
    class function DecToAny(num: integer; NumberSystem: integer): string;
    // 判断一个正整数是否为质数
    class function IsPrime(n: uint64): boolean;
  end;
