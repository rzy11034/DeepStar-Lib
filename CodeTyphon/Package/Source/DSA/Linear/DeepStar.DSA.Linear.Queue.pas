unit DeepStar.DSA.Linear.Queue;

{$mode objfpc}{$H+}
{$ModeSwitch unicodestrings}{$J-}

interface

uses
  Classes,
  SysUtils,
  DeepStar.DSA.Interfaces,
  DeepStar.DSA.Linear.DoubleLinkedList;

type
  generic TQueue<T> = class(TInterfacedObject, specialize IQueue<T>)
  private type
    TDoubleLinkedList = specialize TDoubleLinkedList<T>;

  private
    fData: TDoubleLinkedList;

  public
    constructor Create;
    destructor Destroy; override;
    function Count: integer;
    function IsEmpty: boolean;
    procedure EnQueue(e: T);
    function DeQueue: T;
    function Peek: T;
  end;

implementation

{$I DeepStar.DSA.Linear.Queue_impl}


end.
