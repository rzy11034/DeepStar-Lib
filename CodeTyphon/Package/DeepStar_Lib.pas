{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit DeepStar_Lib;

{$warn 5023 off : no warning about unused units}
interface

uses
  DeepStar.DSA.Interfaces, DeepStar.DSA.Linear.DoubleLinkedList, DeepStar.DSA.Linear.LinkedList, DeepStar.DSA.Linear.Queue, 
  DeepStar.DSA.Linear.Stack, DeepStar.DSA.Strings.RabinKarp, DeepStar.DSA.Hash.HashSet, DeepStar.DSA.Tree.BalanceBinarySearchTree, 
  DeepStar.DSA.Tree.BinarySearchTree, DeepStar.DSA.Tree.BinaryTree, DeepStar.DSA.Tree.Heap, DeepStar.DSA.Tree.PriorityQueue, 
  DeepStar.DSA.Tree.RBTree, DeepStar.DSA.Tree.TreeMap, DeepStar.DSA.Tree.TreeSet, DeepStar.DSA.Tree.Trie, DeepStar.DSA.Tree.AVLTree, 
  DeepStar.DSA.Strings.KMP, DeepStar.Utils, DeepStar.DSA.Hash.HashMap, DeepStar.DSA.Linear.ArrayList, DeepStar.OpenGL.GLAD_GL, DeepStar.OpenGL.GLFW, 
  DeepStar.OpenGL.GLM, DeepStar.OpenGL.Matrix, DeepStar.OpenGL.Vector, DeepStar.OpenGL.Camera, DeepStar.OpenGL.Shader, DeepStar.OpenGL.Texture, 
  DeepStar.OpenGL.Utils, DeepStar.SDL2.Timer, DeepStar.SDL2.Utils, DeepStar.SDL2.Windows, DeepStar.SDL2.Texture, TyphonPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('DeepStar_Lib', @Register);
end.
