  {%MainUint DeepStar.Utils.pas}

  {$IfDef InterfacePart}

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

function IfThen(Condition: boolean; const TrueResult, FalseResult: variant): variant;
  deprecated 'Use IfThen<T> instead';
function CrossFixFileName(const FileName: string): string;

generic function IfThen<T>(Condition: boolean; const TrueResult: T; const FalseResult: T): T; inline;
generic procedure Swap<T>(var a, b: T); inline;

// 交换零基字符串中字符
procedure Swap(var str: string; indexA, indexB: integer);

// 返回 packed record 字段列表的偏移量
function OffsetOf(const Base; const Field): integer;

{$Else}

procedure DrawLineBlockEnd;
var
  i: integer;
begin
  for i := 0 to 70 do
  begin
    Write('─');
  end;
  WriteLn;
end;

procedure DrawLineProgramEnd;
var
  i: integer;
begin
  for i := 0 to 70 do
  begin
    Write('═');
  end;
  WriteLn;
end;

function NeedInput(need: boolean; showText: boolean): boolean;
begin
  if need then
  begin
    if showText then
      WriteLn('Need input data: ');
    Exit(true);
  end;

  Result := false;
end;

function Exp(d: ValReal): ValReal;
begin
  Result := System.Exp(d);
end;

function Chr(i: cardinal): char;
begin
  Result := char(i);
end;

function IfThen(Condition: boolean; const TrueResult, FalseResult: variant): variant;
begin
  Result := Default(variant);

  if Condition then
    Result := TrueResult
  else
    Result := FalseResult;
end;

function CrossFixFileName(const FileName: string): string;
const
  {$IFDEF MSWINDOWS}
  PrevChar = '/';
  NewChar = '\';
  {$ELSE}
  PrevChar = '\';
  NewChar = '/';
  {$ENDIF}
var
  i: integer;
begin
  Result := FileName;
  UniqueString(Result);

  for i := 0 to Result.Length - 1 do
    if Result.Chars[i] = PrevChar then
      Result.Chars[i] := NewChar;
end;

generic function IfThen<T>(Condition: boolean; const TrueResult: T; const FalseResult: T): T;
begin
  Result := Default(T);

  if Condition then
    Result := TrueResult
  else
    Result := FalseResult;
end;

generic procedure Swap<T>(var a, b: T);
var
  temp: T;
begin
  temp := a;
  a := b;
  b := temp;
end;

procedure Swap(var str: string; indexA, indexB: integer);
var
  temp: char;
begin
  temp := str.Chars[indexA];
  str.Chars[indexA] := str.Chars[indexB];
  str.Chars[indexB] := temp;
end;

function OffsetOf(const Base; const Field): integer;
begin
  Result := integer({%H-}UIntPtr(@Field) - {%H-}UIntPtr(@Base));
end;

procedure WriteF(const Fmt: string; const Args: array of const);
begin
  Write(Format(Fmt.ToAnsiString, Args));
end;

procedure WriteLnF(const Fmt: string; const Args: array of const);
begin
  WriteLn(Format(Fmt.ToAnsiString, Args));
end;

{$EndIf}
