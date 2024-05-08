  {%MainUint DeepStar.Utils.pas}

type
  IQueue_int = specialize IQueue<integer>;
  TQueue_int = specialize TQueue<integer>;

  IQueue_chr = specialize IQueue<char>;
  TQueue_chr = specialize TQueue<char>;

  IQueue_str = specialize IQueue<string>;
  TQueue_str = specialize TQueue<string>;
