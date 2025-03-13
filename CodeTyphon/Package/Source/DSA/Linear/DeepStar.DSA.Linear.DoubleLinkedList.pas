unit DeepStar.DSA.Linear.DoubleLinkedList;

{$mode objfpc}{$H+}
{$ModeSwitch unicodestrings}{$J-}

interface

uses
  Classes,
  SysUtils,
  Rtti,
  DeepStar.DSA.Interfaces;

type
  generic TDoubleLinkedList<T> = class(TInterfacedObject, specialize IList<T>)
  public type
    TImpl = specialize TImpl<T>;
    ICmp = TImpl.ICmp;
    TCmp = TImpl.TCmp;

    TNode = class(TObject)
    public
      E: T;
      Prev: TNode;
      Next: TNode;

      constructor Create;
      constructor Create(newE: T);
      destructor Destroy; override;
    end;

  private
    fSize: integer;
    fDummyhead: TNode;
    FdummyTail: TNode;
    fCmp: TImpl.ICmp;

    procedure SetComparer(const newComparer: TImpl.ICmp);
    procedure Swap(var a, b: T);

  public
    constructor Create;
    constructor Create(const arr: array of T);
    constructor Create(comparisonFunc: TImpl.TComparisonFuncs);
    constructor Create(onComparison: TImpl.TOnComparisons);
    constructor Create(cmp: TImpl.ICmp);
    destructor Destroy; override;

    function Contains(e: T): boolean;
    function GetFirst: T;
    function GetItem(index: integer): T;
    function GetLast: T;
    function GetSize: integer;
    function IndexOf(e: T): integer;
    function IsEmpty: boolean;
    function Remove(index: integer): T;
    function RemoveFirst: T;
    function RemoveLast: T;
    function ToArray: TImpl.TArr;
    function ToString: string; reintroduce;
    procedure Add(index: integer; e: T);
    procedure AddFirst(e: T);
    procedure AddLast(e: T);
    procedure AddRange(const arr: array of T);
    procedure AddRange(const arr: array of T; aIndex, aCount: integer);
    procedure Clear;
    procedure RemoveElement(e: T);
    procedure SetItem(index: integer; e: T);
    procedure Reverse;

    property Count: integer read GetSize;
    property Comparer: TImpl.ICmp write SetComparer;
    property Items[i: integer]: T read GetItem write SetItem; default;
  end;

implementation

{$I DeepStar.DSA.Linear.DoubleLinkedList_impl}

end.
