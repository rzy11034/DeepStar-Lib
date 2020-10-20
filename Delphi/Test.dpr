program Test;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  DeepStar.Utils in 'Package\Utils\DeepStar.Utils.pas',
  DeepStar.Utils.UString in 'Package\Utils\DeepStar.Utils.UString.pas',
  DeepStar.DSA.Interfaces in 'Package\DSA\DeepStar.DSA.Interfaces.pas',
  DeepStar.DSA.Hash.Test.HashMap in 'Package\DSA\Hash\Test\DeepStar.DSA.Hash.Test.HashMap.pas',
  DeepStar.DSA.Hash.HashMap in 'Package\DSA\Hash\DeepStar.DSA.Hash.HashMap.pas',
  DeepStar.DSA.Hash.HashSet in 'Package\DSA\Hash\DeepStar.DSA.Hash.HashSet.pas',
  DeepStar.DSA.Linear.Test.ArrayList in 'Package\DSA\Linear\Test\DeepStar.DSA.Linear.Test.ArrayList.pas',
  DeepStar.DSA.Linear.Test.DoubleLinkedList in 'Package\DSA\Linear\Test\DeepStar.DSA.Linear.Test.DoubleLinkedList.pas',
  DeepStar.DSA.Linear.Test.LinkedList in 'Package\DSA\Linear\Test\DeepStar.DSA.Linear.Test.LinkedList.pas',
  DeepStar.DSA.Linear.Test.Queue in 'Package\DSA\Linear\Test\DeepStar.DSA.Linear.Test.Queue.pas',
  DeepStar.DSA.Linear.Test.Stack in 'Package\DSA\Linear\Test\DeepStar.DSA.Linear.Test.Stack.pas',
  DeepStar.DSA.Linear.ArrayList in 'Package\DSA\Linear\DeepStar.DSA.Linear.ArrayList.pas',
  DeepStar.DSA.Linear.DoubleLinkedList in 'Package\DSA\Linear\DeepStar.DSA.Linear.DoubleLinkedList.pas',
  DeepStar.DSA.Linear.LinkedList in 'Package\DSA\Linear\DeepStar.DSA.Linear.LinkedList.pas',
  DeepStar.DSA.Linear.Queue in 'Package\DSA\Linear\DeepStar.DSA.Linear.Queue.pas',
  DeepStar.DSA.Linear.Stack in 'Package\DSA\Linear\DeepStar.DSA.Linear.Stack.pas',
  DeepStar.DSA.Math in 'Package\DSA\Math\DeepStar.DSA.Math.pas',
  DeepStar.DSA.Strings.Test.KMP in 'Package\DSA\Strings\Test\DeepStar.DSA.Strings.Test.KMP.pas',
  DeepStar.DSA.Strings.Test.RabinKarp in 'Package\DSA\Strings\Test\DeepStar.DSA.Strings.Test.RabinKarp.pas',
  DeepStar.DSA.Strings.KMP in 'Package\DSA\Strings\DeepStar.DSA.Strings.KMP.pas',
  DeepStar.DSA.Strings.RabinKarp in 'Package\DSA\Strings\DeepStar.DSA.Strings.RabinKarp.pas',
  DeepStar.DSA.Tree.Test.AVLTree in 'Package\DSA\Tree\Test\DeepStar.DSA.Tree.Test.AVLTree.pas',
  DeepStar.DSA.Tree.Test.BinarySearchTree in 'Package\DSA\Tree\Test\DeepStar.DSA.Tree.Test.BinarySearchTree.pas',
  DeepStar.DSA.Tree.Test.RBTree in 'Package\DSA\Tree\Test\DeepStar.DSA.Tree.Test.RBTree.pas',
  DeepStar.DSA.Tree.Test.TreeMap in 'Package\DSA\Tree\Test\DeepStar.DSA.Tree.Test.TreeMap.pas',
  DeepStar.DSA.Tree.AVLTree in 'Package\DSA\Tree\DeepStar.DSA.Tree.AVLTree.pas',
  DeepStar.DSA.Tree.BalanceBinarySearchTree in 'Package\DSA\Tree\DeepStar.DSA.Tree.BalanceBinarySearchTree.pas',
  DeepStar.DSA.Tree.BinarySearchTree in 'Package\DSA\Tree\DeepStar.DSA.Tree.BinarySearchTree.pas',
  DeepStar.DSA.Tree.BinaryTree in 'Package\DSA\Tree\DeepStar.DSA.Tree.BinaryTree.pas',
  DeepStar.DSA.Tree.Heap in 'Package\DSA\Tree\DeepStar.DSA.Tree.Heap.pas',
  DeepStar.DSA.Tree.RBTree in 'Package\DSA\Tree\DeepStar.DSA.Tree.RBTree.pas',
  DeepStar.DSA.Tree.TreeMap in 'Package\DSA\Tree\DeepStar.DSA.Tree.TreeMap.pas',
  Test.Main in 'Test.Main.pas',
  DeepStar.DSA.Tree.TreeSet in 'Package\DSA\Tree\DeepStar.DSA.Tree.TreeSet.pas',
  DeepStar.DSA.Tree.Test.TreeSet in 'Package\DSA\Tree\Test\DeepStar.DSA.Tree.Test.TreeSet.pas';

begin
  try
    Run;
    DrawLineProgramEnd;
    Writeln(END_OF_PROGRAM_EN);
    Readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
