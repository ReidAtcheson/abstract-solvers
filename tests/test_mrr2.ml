open Hilbert
open Array
open MRR
open FunctionHilbert

module FH = FunctionHilbert
module D = DSL (FH)
module Q = Params (FH)
module R  = MRR (FH)


let fredholm f = fun x -> FH.dot (fun y -> let sinxy = sin (y -. x) in sinxy *. sinxy) f

let a f = fredholm f

let init = fun x -> x
let b =  (fun x -> 1.0)
let p  = {Q.init=init;maxit=5;reltol=1e-6;check_gap=(1);}
let soln = R.solve p a b


(*
let check = modulate soln

let res = D.(norm (check - b) )


let pass = res < 1e-6

let () = if pass then () else print_endline "Conjugate gradient failed on function space example"*)
