{%MainUint DeepStar.Utils.pas}

type
  PString = ^UString;
  UString = unicodestring;
  PChar = ^UChar;
  UChar = unicodechar;

  TUStringHelper = type Helper(TUnicodeStringHelper) for UString
  private type
    TArr_chr = array of UChar;
    TArr_str = array of UString;

  private
    function __getChar(index: integer): UChar;
    function __getLength: integer;
    procedure __setChar(index: integer; const newChars: UChar);

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

    property Chars[index: integer]: UChar read __getChar write __setChar;
    property Length: integer read __getLength;
  end;

  TStringBuilder = TUnicodeStringBuilder;
