unit DeepStar.DSA.Linear.Stack;

{$mode objfpc}{$H+}
{$ModeSwitch unicodestrings}{$J-}

interface

uses
  Classes,
  SysUtils,
  DeepStar.DSA.Interfaces,
  DeepStar.DSA.Linear.DoubleLinkedList;

type
  generic TStack<T> = class(TInterfacedObject, specialize IStack<T>)
  private type
    TDoubleLinkedList = specialize TDoubleLinkedList<T>;

  private
    fData: TDoubleLinkedList;

  public
    constructor Create;
    destructor Destroy; override;

    function Count: integer;
    function IsEmpty: boolean;
    procedure Push(e: T);
    function Pop: T;
    function Peek: T;
  end;

implementation

{$I DeepStar.DSA.Linear.Stack_impl}


end.
