  {%MainUint DeepStar.Utils.pas}

type
  IMap_int_int = specialize IMap<integer, integer>;
  THashMap_int_int = specialize THashMap<integer, integer>;
  TTreeMap_int_int = specialize TTreeMap<integer, integer>;

  IMap_str_int = specialize IMap<string, integer>;
  THashMap_str_int = specialize THashMap<string, integer>;
  TTreeMap_str_int = specialize TTreeMap<string, integer>;

  IMap_chr_int = specialize IMap<string, integer>;
  THashMap_chr_int = specialize THashMap<string, integer>;
  TTreeMap_chr_int = specialize TTreeMap<string, integer>;

  IMap_int_str = specialize IMap<integer, string>;
  THashMap_int_str = specialize THashMap<integer, string>;
  TTreeMap_int_str = specialize TTreeMap<integer, string>;
