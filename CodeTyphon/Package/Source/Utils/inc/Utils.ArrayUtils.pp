  {%MainUint DeepStar.Utils.pas}

  // 数组辅助类
  generic TArrayUtils<T> = class
  private type
    TArr_T = array of T;
    TArr2D_T = array of array of T;
    TArr3D_T = array of array of array of T;
    TArrayHelper_T = specialize TArrayHelper<T>;

  public type
    ICmp_T = specialize IComparer<T>;
    TCmp_T = specialize TComparer<T>;
    TOnComparison_T = specialize TOnComparison<T>;
    TComparisonFunc_T = specialize TComparisonFunc<T>;

  public
    // 快速排序
    class procedure Sort(var arr: TArr_T);
    // 快速排序
    class procedure Sort(var arr: TArr_T; cmp: ICmp_T);
    // 快速排序
    class procedure Sort(var arr: TArr_T; cmp: TOnComparison_T);
    // 快速排序
    class procedure Sort(var arr: TArr_T; cmp: TComparisonFunc_T);
    // 快速排序
    class procedure Sort(var arr: TArr_T; index, Count: integer);
    // 快速排序
    class procedure Sort(var arr: TArr_T; index, Count: integer; cmp: ICmp_T);
    // 快速排序
    class procedure Sort(var arr: TArr_T; index, Count: integer; cmp: TOnComparison_T);
    // 快速排序
    class procedure Sort(var arr: TArr_T; index, Count: integer; cmp: TComparisonFunc_T);
    // 二分查找法
    class function BinarySearch(const arr: TArr_T; const e: T): integer;
    // 二分查找法
    class function BinarySearch(const arr: TArr_T; const e: T; const cmp: ICmp_T): integer;
    // 顺序查找，返回元素e的下标，元素不存在则返回 -1
    class function IndexOf(const arr: TArr_T; e: T): integer;
    // 输出一维数组
    class procedure Print(arr: TArr_T);
    // 输出二维数组
    class procedure Print2D(arr: TArr2D_T; formated: boolean = true);
    // 输出三维数组
    class procedure Print3D(arr: TArr3D_T);
    // 复制一维数组
    class function CopyArray(const arr: TArr_T): TArr_T;
    // 复制二维数组
    class function CopyArray2D(const arr2D: TArr2D_T): TArr2D_T;
    // 填充数组
    class procedure FillArray(var arr: TArr_T; e: T);
    // 反转数组
    class procedure Reverse(var arr: TArr_T);
    // 初始化一维数组并填充默认初始值
    class procedure SetLengthAndFill(var arr: TArr_T; n: integer);
    // 初始化一维数组并填充初始值
    class procedure SetLengthAndFill(var arr: TArr_T; n: integer; f: T);
    // 初始化一维数组并填充默认初始值
    class procedure SetLengthAndFill(var arr: TArr2D_T; n, m: integer);
    // 初始化二维数组并填充初始值
    class procedure SetLengthAndFill(var arr: TArr2D_T; n, m: integer; f: T);
    // 返回一维数组内存区占用大小
    class function MemorySize(const dynArray: TArr_T): integer;
  end;

  TArrayUtils_int = specialize TArrayUtils<integer>;
  TArrayUtils_str = specialize TArrayUtils<UString>;
  TArrayUtils_chr = specialize TArrayUtils<UChar>;
  TArrayUtils_bool = specialize TArrayUtils<boolean>;
  TArrayUtils_dbl = specialize TArrayUtils<double>;



