unit DeepStar.Utils;

{$mode objfpc}{$H+}
{$ModeSwitch unicodestrings}{$J-}
{$modeswitch typehelpers}

interface

uses
  Classes,
  SysUtils,
  Rtti,
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
  {$Include DeepStar.Arrays_h.inc}

  //═════════════════════════════════════════════════════════════════════════
  {$Include DeepStar.UnicodeString_h.inc}
  {$Include DeepStar.ArrayUtils_h.inc}
  {$Include DeepStar.Common_h.inc}

  //════════════════════ 常用容器类特例化定义 ═══════════════════════════════
  {$Include DeepStar.Lists_h.inc}
  {$Include DeepStar.Stacks_h.inc}
  {$Include DeepStar.Queues_h.inc}
  {$Include DeepStar.Maps_h.inc}
  {$Include DeepStar.Sets_h.inc}
  {$Include DeepStar.Heaps_h.inc}
  {$Include DeepStar.PriorityQueues_h.inc}
  //═════════════════════════════════════════════════════════════════════════


implementation


{$Include DeepStar.Common.inc}
{$Include DeepStar.ArrayUtils.inc}
{$Include DeepStar.UnicodeString.inc}

end.
