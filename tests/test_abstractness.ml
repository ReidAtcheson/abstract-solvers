open Hilbert
open Array
open ConjugateGradient
open FunctionHilbert

module FH = FunctionHilbert
module D = DSL (FH)
module Q = Params (FH)
module R  = ConjugateGradient (FH)



let modulate f  = 
  fun x -> -. (x -. 5.0) *. (x +. 5.0) *. (f x)


let a f = modulate f

let init =  fun x -> 1.0 /. ((x -. 1.5) *. (x +. 1.5) )
let b =  (fun x -> 1.0)
let p  = {Q.init=init;maxit=10;reltol=1e-6;check_gap=(15);}
let soln = R.solve p a b



let check = modulate soln

let res = D.(norm (check - b) )


let pass = res < 1e-6

let () = if pass then () else print_endline "Conjugate gradient failed on function space example"
