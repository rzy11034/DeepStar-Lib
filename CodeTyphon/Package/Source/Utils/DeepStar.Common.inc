  {%MainUint DeepStar.Utils.pas}

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

procedure WriteF(const Fmt: string; const Args: array of const);
begin
  Write(Format(Fmt.ToAnsiString, Args));
end;

procedure WriteLnF(const Fmt: string; const Args: array of const);
begin
  WriteLn(Format(Fmt.ToAnsiString, Args));
end;
