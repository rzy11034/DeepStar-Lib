  {%MainUint DeepStar.Utils.pas}

type
  TMath = class(TInterfacedObject)
  private
    class function CharToNum(c: UChar): integer;
    class function NumToChar(n: integer): char;

  public
    // 转换任意进制字符串到十进制数
    class function AnyToDec(numStr: string; NumberSystem: integer): integer;
    // 转换十进制数到任意进制字符串
    class function DecToAny(num: integer; NumberSystem: integer): string;
    // 判断一个正整数是否为质数
    class function IsPrime(n: uint64): boolean;
    // 自定义版本四舍五入
    // RoundMode: TFPURoundingMode = rmNearest 四舍五入
    // RoundMode: TFPURoundingMode = rmDown 四舍五入到小于value的最大整数。
    // RoundMode: TFPURoundingMode = rmUp 四舍五入到比value大的最小整数
    // RoundMode: TFPURoundingMode = rmTruncate 截断小数部分
    class function RoundEx(Value: extended; roundMode: TFPURoundingMode = rmNearest): int64;
  end;


