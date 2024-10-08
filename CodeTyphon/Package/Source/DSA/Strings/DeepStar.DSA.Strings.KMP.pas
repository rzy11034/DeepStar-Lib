﻿unit DeepStar.DSA.Strings.KMP;

{$mode objfpc}{$H+}
{$ModeSwitch unicodestrings}{$J-}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils;

type
  TKMP = class(TInterfacedObject)
  public
    /// <summary> 暴力匹配 </summary>
    class function IndexOf_Simplicity(s, p: string): TArr_int;
    /// <summary> 标准KMP算法——O(m+n) </summary>
    class function IndexOf(s, p: string): TArr_int;
  end;

implementation

{ TKMP }

class function TKMP.IndexOf(s, p: string): TArr_int;
  function __next(p: string): TArr_int;
  var
    Next: TArr_int;
    i, j: integer;
  begin
    // 创建一个 next 数组保存部分匹配值
    SetLength(Next, p.Length);
    Next[0] := 0; // 如果字符串是长度为 1, 部分匹配值就是 0

    i := 1;
    j := 0;
    while i < p.Length do
    begin
      if p.Chars[j] <> p.Chars[i] then
      begin
        Next[i] := 0;
        i += 1;
        j := 0;
      end
      else
      begin
        Next[i] := Next[i - 1] + 1;
        i += 1;
        j += 1;
      end;
    end;

    Result := Next;
  end;

var
  Next, ret: TArr_int;
  i, j: integer;
begin
  if (s.Length = 0) or (p.Length = 0) then
    Exit(nil);
  if (p.Length > s.Length) then
    Exit(nil);


  ret := nil;
  Next := __next(p);

  i := 0; // s位置
  j := 0; // p位置
  while i < s.Length do
  begin
    // 如果j = 0，或者当前字符匹配成功（即 s[i] = p[j]），都令 i+1，j+1
    // j=0，因为next[0]=0,说明p的第一位和 i 这个位置无法匹配，
    // 这时 i，j 都增加1，i移位，j 从 0 开始
    if (j = 0) or (s.Chars[i] = p.Chars[j]) then
    begin
      i += 1;
      j += 1;
    end
    else
    begin
      // 如果 j <> -1，且当前字符匹配失败（即 S[i] <> P[j]），则令 i 不变，j = next[j]
      // next[j] 即为 j 所对应的 next 值
      j := Next[j];
    end;

    if j = p.Length then
    begin
      SetLength(ret, Length(ret) + 1);
      ret[High(ret)] := i - j;

      i -= 1;
      j := Next[j - 1];
    end;
  end;

  Result := ret;
end;

class function TKMP.IndexOf_Simplicity(s, p: string): TArr_int;
var
  s_l, s_r, p_len, p_r: integer;
  ret: TArr_int;
begin
  ret := nil;
  p_len := p.Length;
  s_l := 0;
  s_r := 0;
  p_r := 0;

  while s_r < s.Length do
  begin
    if s.Chars[s_r] = p.Chars[p_r] then
    begin
      s_r += 1;
      p_r += 1;

      if p_r = p_len then
      begin
        SetLength(ret, Length(ret) + 1);
        ret[High(ret)] := s_l;

        s_l += 1;
        s_r := s_l;
        p_r := 0;
      end;
    end
    else
    begin
      s_l += 1;
      s_r := s_l;
      p_r := 0;
    end;
  end;

  Result := ret;
end;


end.
