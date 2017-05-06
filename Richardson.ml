open Hilbert
open Solvers

module Richardson (H : HilbertSpace) 
: Solver with type v = H.v = struct
  module D = DSL (H)
  type v = H.v
  type s = {
    alpha : H.c;
    init  : v;
    maxit : int;
    reltol : H.r;

    check_gap : int;
  }

  let solve p a b = 
    D.(
      let x = ref p.init in
      let res = ref (b - (a !x)) in
      let alpha = p.alpha in 
      let maxit = p.maxit in
      let normb = norm b in
      for i = 1 to maxit do
        x := !x + alpha * (!res);
        res := b - (a !x);
        if (i mod p.check_gap == 0)
        then
          let normr = norm !res in
          Printf.printf "Iteration: %i, Relative Residual: %s" i (string_of_r (normr /.. normb))
        else
          ()
      done;
      !x
    )
end;;
