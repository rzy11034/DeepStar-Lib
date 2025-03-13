unit DeepStar.DSA.Linear.LinkedList;

{$mode objfpc}{$H+}
{$ModeSwitch unicodestrings}{$J-}

interface

uses
  Classes,
  SysUtils,
  Rtti,
  DeepStar.DSA.Interfaces;

type
  generic TLinkedList<T> = class(TInterfacedObject, specialize IList<T>)
  public type
    TImpl = specialize TImpl<T>;
    TArr = TImpl.TArr;
    ICmp = TImpl.ICmp;
    TCmp = TImpl.TCmp;

    TNode = class(TObject)
    public
      E: T;
      Next: TNode;

      constructor Create;
      constructor Create(newE: T; newNext: TNode = nil);
      destructor Destroy; override;
    end;

  private
    fSize: integer;
    fDummyHead: TNode;
    fCmp: TImpl.ICmp;

    procedure fSetComparer(const newComparer: ICmp);

  public
    constructor Create;
    constructor Create(const arr: TArr);
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
    procedure Reverse;
    procedure SetItem(index: integer; e: T);


    property Count: integer read GetSize;
    property Comparer: ICmp write fSetComparer;
    property Items[i: integer]: T read GetItem write SetItem; default;
  end;

implementation

{$I DeepStar.DSA.Linear.LinkedList_impl}

end.
