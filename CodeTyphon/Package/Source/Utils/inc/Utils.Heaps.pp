  {%MainUint DeepStar.Utils.pas}

type
  THeapkind = DeepStar.DSA.Tree.Heap.THeapkind;

  THeap_int = specialize THeap<integer>;
  THeap_int64 = specialize THeap<int64>;
  THeap_chr = specialize THeap<char>;
  THeap_str = specialize THeap<string>;
  THeap_dbl = specialize THeap<double>;