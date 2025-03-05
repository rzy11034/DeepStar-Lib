  {%MainUint DeepStar.Utils.pas}

type
  IStack_int = specialize IStack<integer>;
  TStack_int = specialize TStack<integer>;

  IStack_chr = specialize IStack<char>;
  TStack_chr = specialize TStack<char>;

  IStack_str = specialize IStack<string>;
  TStack_str = specialize TStack<string>;

