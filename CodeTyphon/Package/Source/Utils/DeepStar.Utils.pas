unit DeepStar.Utils;

{$mode objfpc}{$H+}
{$ModeSwitch unicodestrings}{$J-}
{$modeswitch typehelpers}

interface

{$define InterfacePart}

uses
  Classes,
  SysUtils,
  Rtti,
  Math,
  LazUTF8,
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

  // 常用基础类型定义
  {$Include Utils.Types.inc}

  // object 根节点
  {$Include Utils.Object.inc}

  // 常用数组定义
  {$Include Utils.Arrays.inc}

  //═════════════════════════════════════════════════════════════════════════

  {$Include Utils.SysHelper.inc}
  {$Include Utils.ArrayUtils.inc}
  {$Include Utils.Math.inc}
  {$Include Utils.Common.inc}

  //════════════════════ 常用容器类特例化定义 ═══════════════════════════════

  {$Include Utils.Lists.inc}
  {$Include Utils.Stacks.inc}
  {$Include Utils.Queues.inc}
  {$Include Utils.Maps.inc}
  {$Include Utils.Sets.inc}
  {$Include Utils.Heaps.inc}
  {$Include Utils.PriorityQueues.inc}

  //═════════════════════════════════════════════════════════════════════════

  {$undef InterfacePart}

implementation

{$Include Utils.Common.inc}
{$Include Utils.Math.inc}
{$Include Utils.ArrayUtils.inc}
{$Include Utils.SysHelper.inc}

{$Include Utils.Object.inc}

end.
