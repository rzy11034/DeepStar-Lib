unit DeepStar.Utils;

{$mode objfpc}{$H+}

{$I invoke}

interface

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
  {$Include Utils.Types}

  // object 类型根节点
  {$Include Utils.Object}

  // 常用数组定义
  {$Include Utils.Arrays}

  //═════════════════════════════════════════════════════════════════════════

  {$I Utils.SysHelper}
  {$I Utils.ArrayUtils}
  {$I Utils.Math}
  {$I Utils.Common}

  //════════════════════ 常用容器类特例化定义 ═══════════════════════════════

  {$I Utils.Lists}
  {$I Utils.Stacks}
  {$I Utils.Queues}
  {$I Utils.Maps}
  {$I Utils.Sets}
  {$I Utils.Heaps}
  {$I Utils.PriorityQueues}


implementation

  {$I Utils.Common_impl}
  {$I Utils.Math_impl}
  {$I Utils.ArrayUtils_impl}
  {$I Utils.SysHelper_impl}
  {$I Utils.Object_impl}

end.
