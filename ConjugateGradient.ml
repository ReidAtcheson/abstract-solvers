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

module ConjugateGradient (H : HilbertSpace) 
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

  let solve param a b = 
    D.(
      let x = ref param.Q.init in
      let nres = b - (a !x) in
      let res = ref nres in
      let p   = ref nres in
      let maxit = param.Q.maxit in
      let normb = norm b in
      let relres = ref (normb /.. (norm !res)) in
      let i = ref 1 in
      while param.Q.reltol < !relres  && !i <> maxit do
        inc i;
        let ap = a (!p) in
        let anum  = (dot !res !res) in
        let alpha = anum /. (dot !p ap) in
        x := !x + alpha * !p;
        res := !res - (alpha * ap);
        let beta = (dot !res !res) /. anum in
        p := !res + beta * !p;
        relres := (norm !res) /.. (norm b);
        if (!i mod param.Q.check_gap == 0)
        then
          Printf.printf "Iteration: %i, Relative Residual: %s\n" !i (string_of_r !relres)
        else
          ()
      done;
      !x
    )
end;;
