open Array
open ConjugateGradient
open FunctionHilbert

module FH = FunctionHilbert
module Q = Params (FH)
module R  = ConjugateGradient (FH)


(** TODO: needs a good test.. come up with good analytical test *)

let modulate f  = 
  fun x -> -. (x -. 5.0) *. (x +. 5.0) *. (f x)


let a f = modulate f

let init =  fun x -> 1.0 /. ((x -. 1.0) *. (x +. 1.0) )
let b =  (fun x -> 1.0)
let p  = {Q.init=init;maxit=10;reltol=1e-6;check_gap=(1);}
let soln = R.solve p a b


