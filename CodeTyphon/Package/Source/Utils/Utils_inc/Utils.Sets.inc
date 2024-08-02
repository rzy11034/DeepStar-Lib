  {%MainUint DeepStar.Utils.pas}

  {$IfDef InterfacePart}

type
  ISet_str = specialize ISet<string>;
  THashSet_str = specialize THashSet<string>;
  TTreeSet_str = specialize TTreeSet<string>;

  ISet_int = specialize ISet<integer>;
  THashSet_int = specialize THashSet<integer>;
  TTreeSet_int = specialize TTreeSet<integer>;

  ISet_chr = specialize ISet<char>;
  THashSet_chr = specialize THashSet<char>;
  TTreeSet_chr = specialize TTreeSet<char>;

  ISet_dbl = specialize ISet<double>;
  THashSet_dbl = specialize THashSet<double>;
  TTreeSet_dbl = specialize TTreeSet<double>;

  {$EndIf}

