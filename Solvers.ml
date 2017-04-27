open Hilbert 
open Field
open HilbertHelpers



module Richardson 
  (R : OrderedField)
  (C : TopologicalField with type r = R.r)
  (Hl : HilbertSpace_lower with type f = C.c with type r = R.r) = struct

  module D = MakeDSL (R) (C) (Hl)


  let solve a x b alpha maxit reltol = 
    D.(
      let ralpha = RNumber alpha in
      let op = Operator a in
      let xv = ref (Vector x) in
      let bv = Vector b in
      let it = Index (ref 0) in
      let imaxit = Index (ref maxit) in
      let res = ref (bv - op*(!xv)) in
      let normb = norm (bv) in
      let relres = ref ((norm (!res)) / normb) in

      while (it < imaxit && (reltol < !relres) ) do

        xv := !xv + ralpha * !res;
        res := bv - op*(!xv);
        ref := (norm (!res)) / (normb);

        it += 1;
      done;
      0
    )


end;;
