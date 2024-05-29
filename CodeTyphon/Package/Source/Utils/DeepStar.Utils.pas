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

  // 常用数组定义
  {$Include DeepStar.Arrays.inc}

  //═════════════════════════════════════════════════════════════════════════
  {$Include DeepStar.UnicodeStringHelper.inc}
  {$Include DeepStar.ArrayUtils.inc}
  {$Include DeepStar.Math.inc}
  {$Include DeepStar.Common.inc}

  //════════════════════ 常用容器类特例化定义 ═══════════════════════════════
  {$Include DeepStar.Lists.inc}
  {$Include DeepStar.Stacks.inc}
  {$Include DeepStar.Queues.inc}
  {$Include DeepStar.Maps.inc}
  {$Include DeepStar.Sets.inc}
  {$Include DeepStar.Heaps.inc}
  {$Include DeepStar.PriorityQueues.inc}
  //═════════════════════════════════════════════════════════════════════════

  {$undef InterfacePart}

implementation

{$Include DeepStar.Common.inc}
{$Include DeepStar.Math.inc}
{$Include DeepStar.ArrayUtils.inc}
{$Include DeepStar.UnicodeStringHelper.inc}

end.
