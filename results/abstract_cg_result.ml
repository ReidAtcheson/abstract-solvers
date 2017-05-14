open Array
open ConjugateGradient
open FunctionHilbert

module FH = FunctionHilbert
module Q = Params (FH)
module R  = ConjugateGradient (FH)


let soln maxit = 
  let modulate f  = 
    fun x -> -. (x -. 1.0) *. (x +. 1.0) *. (f x) in
  let a f = modulate f in
  let init =  fun x -> 1.0 /. ((x -. 1.0) *. (x +. 1.0) ) in
  let b =  (fun x -> 1.0) in
  let p  = {Q.init=init;maxit=maxit;reltol=1e-6;check_gap=(maxit+1);} in
  R.solve p a b

let arr2file xs fname = 
  let n = length xs in 
  let f = open_out fname in
  for i = 0 to (n-1) do
    Printf.fprintf f "%f\n" xs.(i);
  done;
  close_out f;
;;


let sample f = 
  let n = 100 in
  Array.init n (fun i ->
    let ifl = float_of_int i in
    let nfl = float_of_int n in
    let dx = 2.0 /. nfl in
    let xf = -1.0 +. (ifl +. 1.0) *. dx in
    f xf
  )

(*Now evaluate solution at sampled points*)
let () = 
  for maxit = 1 to 10 do
    let solni = soln maxit in
    let samplesoln = sample solni in
    let outfile = Printf.sprintf "abstract_cg_%i.dat" maxit in
    arr2file samplesoln outfile;
  done;
