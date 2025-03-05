  {%MainUint DeepStar.Utils.pas}

type
  IList_int = specialize IList<integer>;
  TArrayList_int = specialize TArrayList<integer>;
  TLinkedList_int = specialize TLinkedList<integer>;
  TDoubleLinkedList_int = specialize TDoubleLinkedList<integer>;

  IList_Cardinal = specialize IList<Cardinal>;
  TArrayList_Cardinal = specialize TArrayList<Cardinal>;
  TLinkedList_Cardinal = specialize TLinkedList<Cardinal>;
  TDoubleLinkedList_Cardinal = specialize TDoubleLinkedList<Cardinal>;

  IList_str = specialize IList<string>;
  TArrayList_str = specialize TArrayList<string>;
  TLinkedList_str = specialize TLinkedList<string>;
  TDoubleLinkedList_str = specialize TDoubleLinkedList<string>;

  IList_chr = specialize IList<char>;
  TArrayList_chr = specialize TArrayList<char>;
  TLinkedList_chr = specialize TLinkedList<char>;
  TDoubleLinkedList_chr = specialize TDoubleLinkedList<char>;

  IList_dbl = specialize IList<double>;
  TArrayList_dbl = specialize TArrayList<double>;
  TLinkedList_dbl = specialize TLinkedList<double>;
  TDoubleLinkedList_dbl = specialize TDoubleLinkedList<double>;

  IList_Single = specialize IList<Single>;
  TArrayList_Single = specialize TArrayList<Single>;
  TLinkedList_Single = specialize TLinkedList<Single>;
  TDoubleLinkedList_Single = specialize TDoubleLinkedList<Single>;

  //═════════════════════════════════════════════════════════════════════════

  IList_TArr_int = specialize IList<TArr_int>;
  TArrayList_TArr_int = specialize TArrayList<TArr_int>;

  IList_TArr_Cardinal = specialize IList<TArr_Cardinal>;
  TArrayList_TArr_Cardinal = specialize TArrayList<TArr_Cardinal>;
  
  
