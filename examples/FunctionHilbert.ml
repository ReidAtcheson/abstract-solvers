open Array
open Hilbert


module Gquad = Gk.Quad (struct let maxit=100 end)
(** Some helper functions .*)
let cheb n x = cos ((float_of_int n) *. (acos x) )
let chebf n x  = 
  let nf = float_of_int n in
  let theta = acos x in
  sin ((nf +. 1.0) *. theta ) /. (sin theta)
let dcheb n x = 
  let nf = float_of_int n in
  nf *. chebf (n-1) x

let polyval cs x = 
  let n = length cs in
  let out = ref 0.0 in
  for i = 0 to (n-1) do
    out := !out +. cs.(i) *. (cheb i x);
  done;
  !out

let cheb_coeff i f =
  let tol = 1e-6 in
  let chebnorm2 = Gquad.gkint (fun x -> (cheb i x) *. (cheb i x) ) (-1.0) (1.0) tol in
  let chebnorm = sqrt chebnorm2 in
  let integrand x = (f x) *. (cheb i) 1.0 /. ( sqrt (1.0 -. x *. x)) in
  let c = Gquad.gkint integrand (-1.0) (1.0) tol in
  c /. chebnorm


let cheb_exp f = 
  let maxit = 10 in
  let i = ref 0 in
  let tol = 1e-6 in
  let err = ref 1.0 in
  let cs = ref (Array.init 1 (fun x -> 1.0)) in
  while !i < maxit && !err > tol do
    i := !i + 1;
    cs := Array.init !i (fun j -> cheb_coeff j f);     
    let cf = polyval !cs in
    let interr x = ((cf x) -. (f x)) *. ( (cf x) -. (f x)) in
    let err2 = Gquad.gkint interr (-1.0) (1.0) tol in
    err := sqrt err2;
  done;
  !cs

(** Space of functions *)

(** Function is either Ocaml function or 
 * Chebyshev expansion of Ocaml function*)
type aproxfn = 
    Fn of (float -> float)
  | Cheb of (float array)

module FunctionHilbert : 
  HilbertSpace 
    with type v = aproxfn
    with type r = float
    with type c = float
  = struct
  type v = aproxfn
  type r = float
  type c = float

  let left = -1.0
  let right = 1.0
  let tol = 1e-7

  (** Comparisons with real numbers *)
  let lt x y = x<y
  let string_of_r r = Printf.sprintf "%f" r


  let mkf x y = x
  let emb x = x
  let mk = mkf
  let re x = x
  let im x = 0.0
  let mul x y = x *. y
  let add x y = x +. y
  let sub x y = x -. y
  let div x y = x /. y
  let abs x = abs_float x
  let sqrt x = sqrt x


  let len v = match v with
    Fn f -> 0
  | Cheb cs -> Array.length cs
  let getexp v = match v with
    Fn f -> Array.make 1 0.0
  | Cheb cs -> cs
  let expand v = match v with
     Fn f -> Cheb (cheb_exp f)
    |Cheb cs -> Cheb cs
    
  let addv v1 v2 =
    let cv1 = expand v1 in
    let cv2 = expand v2 in
    let cs1 = getexp cv1 in
    let cs2 = getexp cv2 in
    let pv1 = polyval cs1 in
    let pv2 = polyval cs2 in
    let p = fun x -> (pv1 x) +. (pv2 x) in
    Cheb (cheb_exp p)

  let subv v1 v2 = 
    let cv1 = expand v1 in
    let cv2 = expand v2 in
    let cs1 = getexp cv1 in
    let cs2 = getexp cv2 in
    let pv1 = polyval cs1 in
    let pv2 = polyval cs2 in
    let p = fun x -> (pv1 x) -. (pv2 x) in
    Cheb (cheb_exp p)

  let mulv a v   =
    let cv = expand v in
    let cs = getexp cv in
    Cheb (Array.init (Array.length cs) (
      fun i -> (cs.(i) *.  a)
    ))

    
  let divv v a   =  
    let cv = expand v in
    let cs = getexp cv in
    Cheb (Array.init (Array.length cs) (
      fun i -> (cs.(i) /.  a)
    ))

  let dot v1 v2 = 
    let cv1 = expand v1 in
    let cv2 = expand v2 in
    let cs1 = getexp cv1 in
    let cs2 = getexp cv2 in
    let pv1 = polyval cs1 in
    let pv2 = polyval cs2 in
    let p = fun x -> (pv1 x) *. (pv2 x) in
    Gquad.gkint p left right tol

  let norm v = sqrt (dot v v)
end;;

