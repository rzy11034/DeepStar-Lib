unit DeepStar.DSA.Interfaces;

{$mode objfpc}{$H+}
{$ModeSwitch unicodestrings}{$J-}

interface

uses
  Classes,
  SysUtils,
  Generics.Defaults;

type
  generic TImpl<T> = class
  public type
    TArr = array of T;
    TArr2D = array of array of T;

    TCmp = specialize TComparer<T>;
    ICmp = specialize IComparer<T>;
    TOnComparisons = specialize TOnComparison<T>;
    TComparisonFuncs = specialize TComparisonFunc<T>;
  end;

  generic IList<T> = interface
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
    function ToArray: specialize TImpl<T>.TArr;
    function ToString: string;
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
    property Items[i: integer]: T read GetItem write SetItem; default;
  end;

  generic IStack<T> = interface
    ['{F4C21C9B-5BB0-446D-BBA0-43343B7E8A04}']
    function Count: integer;
    function IsEmpty: boolean;
    procedure Push(e: T);
    function Pop: T;
    function Peek: T;
  end;

  generic IQueue<T> = interface
    ['{1454F65C-3628-488C-891A-4A4F6EDECCDA}']
    function Count: integer;
    function IsEmpty: boolean;
    procedure EnQueue(e: T);
    function DeQueue: T;
    function Peek: T;
  end;

  generic ISet<T> = interface
    ['{EB3DEBD8-1473-4AD1-90B2-C5CEF2AD2A97}']
    function Contains(e: T): boolean;
    function Count: integer;
    function IsEmpty: boolean;
    function ToArray: specialize TImpl<T>.TArr;
    procedure Add(e: T);
    procedure Clear;
    procedure Remove(e: T);
  end;

  generic IPtrValue<T> = interface
    ['{1500F8C1-4C32-46D8-B32A-2C2D5FE43A18}']
    function Value: T;
  end;

  generic TPtrValue<T> = class(TInterfacedObject, specialize IPtrValue<T>)
  private
    _Value: T;
  public
    function Value: T;
    constructor Create(newValue: T);
  end;

  generic IMap<K, V> = interface
    ['{4D344A23-A724-4120-80D8-C7F07F33D367}']
    function Add(key: K; Value: V): specialize IPtrValue<V>;
    function ContainsKey(key: K): boolean;
    function ContainsValue(Value: V): boolean;
    function Count: integer;
    function GetItem(key: K): V;
    function IsEmpty: boolean;
    function Keys: specialize TImpl<K>.TArr;
    function Remove(key: K): specialize IPtrValue<V>;
    function Values: specialize  TImpl<V>.TArr;
    procedure Clear;
    procedure SetItem(key: K; Value: V);
    property Item[key: K]: V read GetItem write SetItem; default;
  end;

type
  TStringBuilder = TUnicodeStringBuilder;

implementation

{ TPtr_V }

constructor TPtrValue.Create(newValue: T);
begin
  _Value := newValue;
end;

function TPtrValue.Value: T;
begin
  Result := _Value;
end;

end.
