  {%MainUint DeepStar.Utils.pas}

  {$IfDef InterfacePart}

type
  TPriorityQueue_int = specialize TPriorityQueue<integer>;
  TPriorityQueue_chr = specialize TPriorityQueue<char>;
  TPriorityQueue_str = specialize TPriorityQueue<string>;
  TPriorityQueue_dbl = specialize TPriorityQueue<double>;

  {$EndIf}

