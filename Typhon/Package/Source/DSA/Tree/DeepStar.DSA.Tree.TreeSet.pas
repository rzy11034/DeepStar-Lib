unit DeepStar.DSA.Tree.TreeSet;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.DSA.Interfaces,
  DeepStar.DSA.Tree.TreeMap;

type
  generic TTreeSet<T> = class(TInterfacedObject, specialize ISet<T>)
  private type
    TImpl_T = specialize TImpl<T>;
    TTreeMap = specialize TTreeMap<T, TObject>;
    TTreeSet_T = specialize TTreeSet<T>;

  public type
    TPtrValue_T = specialize TPtrValue<T>;

  private
    _data: TTreeMap;

    function __getCmp_T: TImpl_T.ICmp;
    procedure __setCmp_T(const newComparer: TImpl_T.ICmp);

  public
    constructor Create;
    destructor Destroy; override;

    function Clone: TTreeSet_T;
    function Contains(e: T): boolean;
    function Count: integer;
    function IsEmpty: boolean;
    function ToArray: TImpl_T.TArr;
    procedure Add(e: T);
    procedure AddAll(treeSet: TTreeSet_T);
    procedure Clear;
    procedure Remove(e: T);
    function Ceiling(e: T): TPtrValue_T;
    function Floor(e: T): TPtrValue_T;

    property Comparer: TImpl_T.ICmp read __getCmp_T write __setCmp_T;
  end;

implementation

{ TTreeSet }

constructor TTreeSet.Create;
begin
  _data := TTreeMap.Create;
end;

procedure TTreeSet.Add(e: T);
begin
  _data.Add(e, nil);
end;

procedure TTreeSet.AddAll(treeSet: TTreeSet_T);
var
  e: T;
begin
  for e in treeSet.ToArray do
  begin
    Self.Add(e);
  end;
end;

function TTreeSet.Ceiling(e: T): TPtrValue_T;
begin
  Result := _data.Ceiling(e);
end;

procedure TTreeSet.Clear;
begin
  _data.Clear;
end;

function TTreeSet.Clone: TTreeSet_T;
var
  res: TTreeSet_T;
  e: T;
begin
  res := TTreeSet_T.Create;

  for e in Self.ToArray do
  begin
    res.Add(e);
  end;

  Result := res;
end;

function TTreeSet.Contains(e: T): boolean;
begin
  Result := _data.ContainsKey(e);
end;

function TTreeSet.Count: integer;
begin
  Result := _data.Count;
end;

destructor TTreeSet.Destroy;
begin
  _data.Free;
  inherited Destroy;
end;

function TTreeSet.Floor(e: T): TPtrValue_T;
begin
  Result := _data.Floor(e);
end;

function TTreeSet.IsEmpty: boolean;
begin
  Result := _data.IsEmpty;
end;

procedure TTreeSet.Remove(e: T);
begin
  _data.Remove(e);
end;

function TTreeSet.ToArray: TImpl_T.TArr;
begin
  Result := _data.Keys;
end;

function TTreeSet.__getCmp_T: TImpl_T.ICmp;
begin
  Result := _data.Comparer_K;
end;

procedure TTreeSet.__setCmp_T(const newComparer: TImpl_T.ICmp);
begin
  _data.Comparer_K := newComparer;
end;

end.
