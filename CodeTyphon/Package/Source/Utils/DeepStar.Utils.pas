unit DeepStar.Utils;

{$mode objfpc}{$H+}
{$ModeSwitch unicodestrings}{$J-}
{$modeswitch typehelpers}

interface

uses
  Classes,
  SysUtils,
  Rtti,
  Generics.Defaults,
  Generics.Collections,
  DeepStar.DSA.Interfaces,
  DeepStar.DSA.Tree.TreeMap,
  DeepStar.DSA.Tree.TreeSet,
  DeepStar.DSA.Hash.HashMap,
  DeepStar.DSA.Hash.HashSet,
  DeepStar.DSA.Linear.ArrayList,
  DeepStar.DSA.Linear.LinkedList,
  DeepStar.DSA.Linear.DoubleLinkedList,
  DeepStar.DSA.Linear.Queue,
  DeepStar.DSA.Linear.Stack,
  DeepStar.DSA.Tree.Heap,
  DeepStar.DSA.Tree.PriorityQueue;

  // 常用数组定义
  {$Include DeepStar.Arrays_h.inc}

  //═════════════════════════════════════════════════════════════════════════
  {$Include DeepStar.UnicodeString_h.inc}
  {$Include DeepStar.ArrayUtils_h.inc}

  //════════════════════ 常用容器类特例化定义 ═══════════════════════════════
  {$Include DeepStar.Lists_h.inc}
  {$Include DeepStar.Stacks_h.inc}
  {$Include DeepStar.Queues_h.inc}
  {$Include DeepStar.Maps_h.inc}
  {$Include DeepStar.Sets_h.inc}
  {$Include DeepStar.Heaps_h.inc}
  {$Include DeepStar.PriorityQueues_h.inc}
  //═════════════════════════════════════════════════════════════════════════

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

implementation

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

{$Include DeepStar.ArrayUtils.inc}
{$Include DeepStar.UnicodeString.inc}

end.
