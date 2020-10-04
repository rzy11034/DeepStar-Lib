unit DeepStar.DSA.Hash.HashSet;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.DSA.Hash.HashMap,
  DeepStar.DSA.Interfaces;

type
  generic THashSet<T> = class(TInterfacedObject, specialize ISet<T>)
  private type
    TImpl_T = specialize TImpl<T>;
    THashMap = specialize THashMap<T, TObject>;
    THashSet_T = specialize THashSet<T>;

  private
    _data: THashMap;

    function __getCmp_T: TImpl_T.ICmp;
    procedure __setCmp_T(const newComparer: TImpl_T.ICmp);

  public
    constructor Create;
    destructor Destroy; override;

    function Clone: THashSet_T;
    function Contains(e: T): boolean;
    function Count: integer;
    function IsEmpty: boolean;
    function ToArray: TImpl_T.TArr;
    procedure Add(e: T);
    procedure AddAll(hashSet: THashSet_T);
    procedure Clear;
    procedure Remove(e: T);

    property Comparer: TImpl_T.ICmp read __getCmp_T write __setCmp_T;
  end;

implementation

{ THashSet }

constructor THashSet.Create;
begin
  _data := THashMap.Create;
end;

procedure THashSet.Add(e: T);
begin
  _data.Add(e, nil);
end;

procedure THashSet.AddAll(hashSet: THashSet_T);
var
  e: T;
begin
  for e in hashSet.ToArray do
  begin
    Self.Add(e);
  end;
end;

procedure THashSet.Clear;
begin
  _data.Clear;
end;

function THashSet.Clone: THashSet_T;
var
  res: THashSet_T;
  e: T;
begin
  res := THashSet_T.Create;

  for e in Self.ToArray do
  begin
    res.Add(e);
  end;

  Result := res;
end;

function THashSet.Contains(e: T): boolean;
begin
  Result := _data.ContainsKey(e);
end;

function THashSet.Count: integer;
begin
  Result := _data.Count;
end;

destructor THashSet.Destroy;
begin
  _data.Free;
  inherited Destroy;
end;

function THashSet.IsEmpty: boolean;
begin
  Result := _data.IsEmpty;
end;

procedure THashSet.Remove(e: T);
begin
  _data.Remove(e);
end;

function THashSet.ToArray: TImpl_T.TArr;
begin
  Result := _data.Keys;
end;

function THashSet.__getCmp_T: TImpl_T.ICmp;
begin
  Result := _data.Comparer_K;
end;

procedure THashSet.__setCmp_T(const newComparer: TImpl_T.ICmp);
begin
  _data.Comparer_K := newComparer;
end;

end.
