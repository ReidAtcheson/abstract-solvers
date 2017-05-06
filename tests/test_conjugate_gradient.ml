
open ConjugateGradient
open ArrayHilbert


module AH = ArrayHilbert
module Q = Params (AH)
module R  = ConjugateGradient (AH)

let n = 32
let pi = 4.0 *. atan (1.0)
let z = Array.make n 0.0

let b = Array.init n (
  fun i -> 
    let ifl = float_of_int (i+1) in
    let dx = pi /. (float_of_int (n + 1)) in
    sin (ifl *. dx)
)

let a x =
  let y = Array.copy x in
  let dx = 1.0 /. (float_of_int (n + 1)) in
  y.(0) <- (2.0 *. x.(0) -. x.(1)) /. (dx *. dx);
  y.(n-1) <-(2.0 *. x.(n-1) -. x.(n-2)) /. (dx *. dx);
  for i = 1 to (n-2) do
    y.(i) <- (2.0 *. x.(i) -. x.(i-1) -. x.(i+1)) /. (dx *. dx);
  done;
  y

let p  = { Q.init=z;maxit=5;reltol=1e-6;check_gap=(6);}

let soln = R.solve p a b

let () = 
  let tol=1e-3 in
  for i = 0 to (n-1) do
   (* let x = abs_float soln.(i) in*)
    let x =  soln.(i) in
    let y = b.(i) in
    let err = (abs_float ((pi *. pi *. x) -. y)) /. y in
    if err > tol
    then
    Printf.printf "Conjugate Gradient iteration failed on float-arrays!\n"
    else
      ()
   done;
