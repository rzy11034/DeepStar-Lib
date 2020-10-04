unit DeepStar.DSA.Interfaces;

{$mode delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  Generics.Defaults,
  DeepStar.Utils.UString;

type
  TImpl<T> = class
  public type
    TArr = array of T;
    TArr2D = array of array of T;

    TCmp = TComparer<T>;
    ICmp = IComparer<T>;
    TOnComparisons = TOnComparison<T>;
    TComparisonFuncs = TComparisonFunc<T>;
  end;

  IList<T> = interface
    ['{9D4D55EE-BC63-49D0-BE20-559D3F82E651}']
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
    function ToArray:  TImpl<T>.TArr;
    function ToString: UString;
    procedure Add(index: integer; e: T);
    procedure AddFirst(e: T);
    procedure AddLast(e: T);
    procedure AddRange(const arr: array of T);
    procedure Clear;
    procedure RemoveElement(e: T);
    procedure SetItem(index: integer; e: T);
    property Count:integer read GetSize;
    property Items[i: integer]: T read GetItem write SetItem; default;
  end;

  IStack<T> = interface
    ['{F4C21C9B-5BB0-446D-BBA0-43343B7E8A04}']
    function Count: integer;
    function IsEmpty: boolean;
    procedure Push(e: T);
    function Pop: T;
    function Peek: T;
  end;

   IQueue<T> = interface
    ['{1454F65C-3628-488C-891A-4A4F6EDECCDA}']
    function Count: integer;
    function IsEmpty: boolean;
    procedure EnQueue(e: T);
    function DeQueue: T;
    function Peek: T;
  end;

  ISet<T> = interface
    ['{EB3DEBD8-1473-4AD1-90B2-C5CEF2AD2A97}']
    function Contains(e: T): boolean;
    function Count: integer;
    function IsEmpty: boolean;
    function ToArray: TImpl<T>.TArr;
    procedure Add(e: T);
    procedure Clear;
    procedure Remove(e: T);
  end;

  TPtrValue<V> = class
  public
    Value: V;
    constructor Create(newValue: V);
  end;

  IMap<K, V> = interface
    ['{4D344A23-A724-4120-80D8-C7F07F33D367}']
    function Add(key: K; Value: V):  TPtrValue<V>;
    function ContainsKey(key: K): boolean;
    function ContainsValue(value: V): boolean;
    function Count: integer;
    function GetItem(key: K): V;
    function IsEmpty: boolean;
    function Keys: TImpl<K>.TArr;
    function Remove(key: K):  TPtrValue<V>;
    function Values: TImpl<V>.TArr;
    procedure Clear;
    procedure SetItem(key: K; Value: V);
  end;

implementation

{ TPtr_V }

constructor TPtrValue<V>.Create(newValue: V);
begin
  Value := newValue;
end;

end.
