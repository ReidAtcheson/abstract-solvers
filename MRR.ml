open Hilbert
open Solvers

module Params (H : HilbertSpace) = struct
  type c = H.c
  type v = H.v
  type r = H.r
  type s = {
    init  : v;
    maxit : int;
    reltol : r;
    check_gap : int;
  }
end;;

module MRR (H : HilbertSpace) 
: Solver 
with type v = H.v 
with type r = H.r
with type c = H.c
with type s = Params(H).s
= struct
  module D = DSL (H)
  module Q = Params (H)
  type v = H.v
  type r = H.r
  type c = H.c
  type s = Q.s

  let inc i = i  := (!i) + 1

  let solve p a b = 
    D.(
      let x = ref p.Q.init in
      let res = ref (b - (a !x)) in
      let alpha = ref ((dot !res (a !res)) /. (dot (a !res) (a !res))) in
      let maxit = p.Q.maxit in
      let normb = norm b in
      let relres = ref (normb /.. (norm !res)) in
      let i = ref 1 in
      while p.Q.reltol < !relres  && !i <> maxit do
        inc i;
        alpha := ((dot !res (a !res)) /. (dot (a !res) (a !res)));
        x := !x + !alpha * (!res);
        res := b - (a !x);
        if (!i mod p.Q.check_gap == 0)
        then
          let normr = norm !res in
          relres := normr /.. normb;
          Printf.printf "Iteration: %i, Relative Residual: %s\n" !i (string_of_r !relres)
        else
          ()
      done;
      !x
    )
end;;
