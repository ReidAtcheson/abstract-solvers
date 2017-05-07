open Array
open ConjugateGradient
open FunctionHilbert

module FH = FunctionHilbert
module Q = Params (FH)
module R  = ConjugateGradient (FH)



let diff f x = 
  let h = 1e-8 in
  let fp1 = if abs_float (x +.h -. 1.0) < h then 0.0 else f (x +. h) in
  let fm1 = if abs_float (x -.h +. 1.0) < h then 0.0 else f (x +. h) in
  let diff_f = fp1 -. fm1 in
  diff_f /. (2.0 *. h)


let a f = 
  diff f

let init = Fn (fun x -> 1.0)
let b = Fn (fun x -> 1.0)
let p  = { Q.init=init;maxit=2;reltol=1e-6;check_gap=(1);}
let soln = R.solve p a b


