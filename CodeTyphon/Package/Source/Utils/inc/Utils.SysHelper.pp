  {%MainUint DeepStar.Utils.pas}

type
  UChar = unicodechar;
  PChar = ^UChar;
  PString = ^UString;
  UString = unicodestring;

  TUnicodeStringHelper = type Helper(SysUtils.TUnicodeStringHelper) for UString
  private type
    TArr_chr = array of UChar;
    TArr_str = array of UString;

  private
    function GetChar(index: integer): UChar;
    function GetLength: integer;
    procedure SetChar(index: integer; const newChars: UChar);

  public
    constructor Create(const chrArr: TArr_chr);
    constructor Create(const chrArr: TArr_chr; startIndex, len: integer);

    procedure Format(const args: array of const);
    function PadLeft(toltaWidth: integer; PaddingChar: UChar): UString;
    function ReverseString: UString;
    function Split(const Separators: TArr_chr): TArr_str;
    function Substring(index: integer): UString;
    function Substring(index: integer; len: integer): UString;
    function ToAnsiString: AnsiString;
    function ToCharArray: TArr_chr;
    function ToInteger: integer;
    function ToPAnsiChar: PAnsiChar; inline;
    function Trim: UString;

    property Chars[index: integer]: UChar read GetChar write SetChar;
    property Length: integer read GetLength;
  end;

  //═════════════════════════════════════════════════════════════════════════

  TStringHelper = type Helper(SysUtils.TStringHelper) for AnsiString
  public
    function ToUString: String;
  end;

  //═════════════════════════════════════════════════════════════════════════

  TStringBuilder = TUnicodeStringBuilder;

  //═════════════════════════════════════════════════════════════════════════

  TIntegerHelper = Type Helper(SysUtils.TIntegerHelper) for Integer
  public
    function ToBinString:string;
    function ToHexString(const AMinDigits: Integer): string;
    function ToHexString: string;
    function ToString: String;
  end;

  //═════════════════════════════════════════════════════════════════════════

  TCardinalHelper = Type Helper(SysUtils.TCardinalHelper) for Cardinal
  public
    function ToBinString:string;
    function ToHexString(const AMinDigits: Integer): string;
    function ToHexString: string;
    function ToString: String;
  end;

  //═════════════════════════════════════════════════════════════════════════

  TLongIntHelper = Type Helper(SysUtils.TLongIntHelper) for LongInt
  public
    function ToBinString:string;
    function ToHexString(const AMinDigits: Integer): string;
    function ToHexString: string;
    function ToString: String;
  end;

  //═════════════════════════════════════════════════════════════════════════

  TInt64Helper = Type Helper(SysUtils.TInt64Helper) for Int64
  public
    function ToString: String;
  end;

  //═════════════════════════════════════════════════════════════════════════

  { TSingleHelper }

  TSingleHelper = Type Helper(SysUtils.TSingleHelper) for Single
  public
    function Round: int64;
    function Trunc: int64;
    function ToString(const AFormat: TFloatFormat; const APrecision, ADigits: Integer): string;
    function ToString(const AFormat: TFloatFormat; const APrecision, ADigits: Integer;
      const AFormatSettings: TFormatSettings): string;
    function ToString(const AFormatSettings: TFormatSettings): string;
    function ToString: string;
  end;

  //═════════════════════════════════════════════════════════════════════════

  { TDoubleHelper }

  TDoubleHelper = Type Helper(SysUtils.TDoubleHelper) for Double
  public
    function Round: int64;
    function Trunc: int64;
    function ToString(const AFormat: TFloatFormat; const APrecision, ADigits: Integer): string;
    function ToString(const AFormat: TFloatFormat; const APrecision, ADigits: Integer;
      const AFormatSettings: TFormatSettings): string;
    function ToString(const AFormatSettings: TFormatSettings): string;
    function ToString: string;
  end;

  //═════════════════════════════════════════════════════════════════════════

  TBooleanHelper = Type Helper(SysUtils.TBooleanHelper) for Boolean
  public
    function ToString(UseBoolStrs: TUseBoolStrs = TUseBoolStrs.False): string;
  end;


