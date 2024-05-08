unit DeepStar.DSA.Tree.Trie;

{$mode objfpc}{$H+}
{$ModeSwitch unicodestrings}{$J-}

interface

uses
  Classes,
  SysUtils,
  DeepStar.DSA.Interfaces,
  DeepStar.DSA.Hash.HashMap;

type
  generic TTrie<V> = class
  private type
    THashMap = specialize THashMap<char, V>;
    TImpl = specialize TImpl<char>;
    TPtrValue = specialize TPtrValue<V>;

    TNode = class
    public
      Next: THashMap;
      IsWord: boolean;
      Value: V;

      constructor Create;
      destructor Destroy; override;
    end;

  private
    _root: TNode;
    _size: integer;

  public
    constructor Create;
    destructor Destroy; override;

    // 获取存储的单词数量
    function Count: integer;
    // 是否为空
    function IsEmpty: boolean;
    // 查询单词是否存在
    function Contains(str: string): boolean;
    // 清楚
    procedure Clear;
    // 添加一个新的单词
    function Add(str: string): TPtrValue;
    // 删除一个新的单词
    function Remove(str: string): TPtrValue;
    // 是否有单词以prefix为前缀
    function IsPerFix(prefix: string): boolean;
  end;

implementation

uses
  DeepStar.Utils;

  { TTrie }

constructor TTrie.Create;
begin
  _root := TNode.Create;
  _size := 0;
end;

function TTrie.Add(str: string): TPtrValue;
var
  cur: TNode;
begin
  if str = '' then
    raise Exception.Create('string must not be empty!');
end;

procedure TTrie.Clear;
begin
  _root.Next.Clear;
  _size := 0;
end;

function TTrie.Contains(str: string): boolean;
begin
end;

function TTrie.Count: integer;
begin
  Result := _size;
end;

destructor TTrie.Destroy;
begin
  Clear;
  FreeAndNil(_root);
  inherited Destroy;
end;

function TTrie.IsEmpty: boolean;
begin
  Result := _size = 0;
end;

function TTrie.IsPerFix(prefix: string): boolean;
begin
end;

function TTrie.Remove(str: string): TPtrValue;
begin
end;

{ TTrie.TNode }

constructor TTrie.TNode.Create;
begin
  IsWord := false;
  Next := THashMap.Create;
end;

destructor TTrie.TNode.Destroy;
begin
  inherited Destroy;
end;

end.
