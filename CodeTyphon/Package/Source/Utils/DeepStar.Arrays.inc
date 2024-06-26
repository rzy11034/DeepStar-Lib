﻿  {%MainUint DeepStar.Utils.pas}

  {$IfDef InterfacePart}
type
  // 一维数组
  TArr_int = array of integer;
  TArr_int64 = array of int64;
  TArr_chr = array of char;
  TArr_str = array of string;
  TArr_bool = array of boolean;
  TArr_dbl = array of double;

  // 二维数组
  TArr2D_int = array of array of integer;
  TArr2D_int64 = array of array of int64;
  TArr2D_chr = array of array of char;
  TArr2D_str = array of array of string;
  TArr2D_bool = array of array of boolean;
  TArr2D_dbl = array of array of double;

  // 三维数组
  TArr3D_int = array of array of array of integer;

  {$Else}

  {$EndIf}
