  {%MainUint DeepStar.Utils.pas}

resourcestring
  END_OF_PROGRAM_EN = 'Press <ENTER> key to continue...';
  END_OF_PROGRAM_CH = '按 <ENTER> 键继续...';

procedure DrawLineBlockEnd;
procedure DrawLineProgramEnd;

function NeedInput(need: boolean = true; showText: boolean = true): boolean;
function Exp(d: ValReal): ValReal;
function Chr(i: cardinal): char;

procedure WriteF(const Fmt: string; const Args: array of const);
procedure WriteLnF(const Fmt: string; const Args: array of const);

function CrossFixFileName(const FileName: string): string;

generic function IfThen<T>(Condition: boolean; const TrueResult: T; const FalseResult: T): T;
generic procedure Swap<T>(var a, b: T); inline;

// 交换零基字符串中字符
procedure Swap(var str: string; indexA, indexB: integer);

// 返回 packed record 字段列表的偏移量
function OffsetOf(const Base; const Field): Cardinal;


