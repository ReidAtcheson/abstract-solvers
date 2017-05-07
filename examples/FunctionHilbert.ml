open Array
open Hilbert


module Gquad = Gk.Quad (struct let maxit=100 end)
(** Some helper functions .*)
(** Space of functions *)


module FunctionHilbert : 
  HilbertSpace 
    with type v = float -> float
    with type r = float
    with type c = float
  = struct
  type v = float -> float
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


    
  let addv v1 v2 = fun x -> (v1 x) +. (v2 x)

  let subv v1 v2 = fun x -> (v1 x) -. (v2 x)

  let mulv a v   = fun x -> a *. (v x)
    
  let divv v a   = fun x -> (v x) /. a
  let dot v1 v2 = 
    let v = fun x -> (v1 x) *. (v2 x) in
    Gquad.gkint v left right tol

  let norm v = sqrt (dot v v)
end;;

