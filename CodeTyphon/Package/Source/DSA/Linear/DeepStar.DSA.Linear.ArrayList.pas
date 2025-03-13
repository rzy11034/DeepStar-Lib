unit DeepStar.DSA.Linear.ArrayList;

{$mode objfpc}{$H+}
{$ModeSwitch unicodestrings}{$J-}

interface

uses
  Classes,
  SysUtils,
  Rtti,
  DeepStar.DSA.Interfaces;

type
  generic TArrayList<T> = class(TInterfacedObject, specialize IList<T>)
  public type
    TImpl = specialize TImpl<T>;
    TArr = TImpl.TArr;
    ICmp = TImpl.ICmp;
    TCmp = TImpl.TCmp;
    PT = ^T;

  private
    fData: TArr;
    fCapacity: uint64;
    fSize: integer;
    fCmp: TImpl.ICmp;

    function GetItemPtr(index: integer): PT;
    procedure QuickSort(l, r: integer);
    procedure ResizeIncreases;
    procedure ResizeDecreases;
    procedure SetComparer(const newComparer: TImpl.ICmp);
    procedure Swap(var a, b: T);

  public
    // 构造函数，传入数组的容量 capacity 构造 TArrayList
    // 默认数组的容量capacity:=10
    constructor Create(capacity: integer = 4);
    // 构造函数，传入数组构造 TArrayList
    constructor Create(const arr: TArr);
    // 构造函数，传入 TComparisonFunc。
    constructor Create(comparisonFunc: TImpl.TComparisonFuncs);
    // 构造函数，传入 TOnComparisons。
    constructor Create(onComparison: TImpl.TOnComparisons);
    // 构造函数，传入 IComparer
    constructor Create(cmp: TImpl.ICmp);
    destructor Destroy; override;

    // 获取数组中的元数个数
    function GetSize: integer;
    // 获取数组的容量
    function GetCapacity: integer;
    // 返回数组是否有空
    function IsEmpty: boolean;
    // 获取index索引位置元素
    function GetItem(index: integer): T;
    // 获取第一个元素
    function GetFirst: T;
    // 获取最后一个元素
    function GetLast: T;
    // 修改index索引位置元素
    procedure SetItem(index: integer; e: T);
    // 向所有元素后添加一个新元素
    procedure AddLast(e: T);
    // 在第index个位置插入一个新元素e
    procedure Add(index: integer; e: T);
    // 在所有元素前添加一个新元素
    procedure AddFirst(e: T);
    // 添加数组所有元素
    procedure AddRange(const arr: array of T);
    // 添加数组aIndex开始共aCount个元素
    procedure AddRange(const arr: array of T; aIndex, aCount: integer);
    // 查找数组中是否有元素e
    function Contains(e: T): boolean;
    // 查找数组中元素e忆的索引，如果不存在元素e，则返回-1
    function IndexOf(e: T): integer;
    // 从数组中删除index位置的元素，返回删除的元素
    function Remove(index: integer): T;
    // 从数组中删除第一个元素，返回删除的元素
    function RemoveFirst: T;
    // 从数组中删除i最后一个元素，返回删除的元素
    function RemoveLast: T;
    // 从数组中删除元素e
    procedure RemoveElement(e: T);
    // 排序
    procedure Sort;
    // 返回一个数组
    function ToArray: TArr;
    // 清空列表
    procedure Clear;
    // 反转列表
    procedure Reverse;

    function ToString: string; reintroduce;

    property Count: integer read GetSize;
    property Comparer: TImpl.ICmp write SetComparer;
    property Items[i: integer]: T read GetItem write SetItem; default;
    property ItemPtr[i: integer]: PT read GetItemPtr;
  end;

implementation

{$I DeepStar.DSA.Linear.ArrayList_impl}

end.
