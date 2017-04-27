open Hilbert
open Field

module MakeRealHilbert 
  (R : OrderedField)
  (H : HilbertSpace_lower with type f = R.r with type r = R.r)
  : HilbertSpace with type v = H.v with type f = H.f with type r = H.r = struct
  type v = H.v
  type f = H.f
  type r = H.r

  let zero = H.zero ()
  let add v1 v2 =
    let res =  H.zero () in
    let () = H.copy res v1 in
    let () = H.axpy res (R.mul_id) v2 in
    res
  let smul v1 a = 
    let res = H.zero () in
    let () = H.copy res v1 in
    let () = H.scale res a in
    res
  let dot v1 v2 = H.dot v1 v2
  let norm v = H.norm v
end;;


module MakeComplexHilbert 
  (R : OrderedField)
  (C : TopologicalField with type r = R.r)
  (H : HilbertSpace_lower with type f = C.c with type r = R.r)
  : HilbertSpace with type v = H.v with type f = H.f with type r = H.r = struct
  type v = H.v
  type f = H.f
  type r = H.r

  let zero = H.zero ()
  let add v1 v2 =
    let res =  H.zero () in
    let () = H.copy res v1 in
    let () = H.axpy res (C.mul_id) v2 in
    res
  let smul v1 a = 
    let res = H.zero () in
    let () = H.copy res v1 in
    let () = H.scale res a in
    res
  let dot v1 v2 = H.dot v1 v2
  let norm v = H.norm v
end;;



module MakeDSL
  (R : OrderedField)
  (C : TopologicalField with type r = R.r)
  (Hl : HilbertSpace_lower with type f = C.c with type r = R.r) = struct
  type v = Hl.v
  type f = Hl.f
  type r = Hl.r
  module H = MakeComplexHilbert (R) (C) (Hl)
  type t = 
    Vector of v |
    Operator of (v->v) |
    Number   of f |
    RNumber  of r |
    Index    of int ref


  let vect_of_v v = Vector v
  let op_of_v2v v2v = Operator v2v
  let num_of_c c = Number c
  let rnum_of_r r = RNumber r

  let r_of_rnum r = match r with
     RNumber r -> r
    |_         -> raise (Failure "Conversion from non-RNumber to type r")

  let c_of_num c = match c with
    Number c -> c
  |_         -> raise (Failure "Conversion from non-Number to type c")

  let v2v_of_op op = match op with
    Operator op -> op
  |_            -> raise (Failure "Conversion from non-Operator to type v->v")


  let v_of_vect vect = match vect with
    Vector v -> v
  |_         -> raise (Failure "Conversion from non-Vector to type v")


  let ( * ) x y = match x,y with
  | Vector xx, Number f  |Number f, Vector xx -> Vector (H.smul xx f)
  | Vector xx, RNumber r  |RNumber r, Vector xx -> Vector (H.smul xx (C.mk r R.add_id))
  | Operator a, Vector yy -> Vector (a yy)
  | Operator a, Operator b -> Operator (fun x -> (a (b x)))
  | Operator a, Number f  | Number f, Operator a -> Operator (fun x -> H.smul x f)
  | Operator a, RNumber r  | RNumber r, Operator a -> Operator (fun x -> H.smul x (C.mk r R.add_id))
  | Number f1, Number f2   -> Number (C.mul f1 f2)
  | RNumber r, Number f | Number f, RNumber r -> Number (C.mul f (C.mk r R.add_id))
  | RNumber r1, RNumber r2 -> RNumber (R.mul r1 r2)
  | _,_                   ->raise (Failure "Attempt to add incompatible types")


  let ( / ) x y = match x,y with
    Vector xx, Number f -> Vector (H.smul xx (C.mul_inv f))
  | Vector xx, RNumber r -> Vector (H.smul xx (C.mul_inv (C.mk r (R.add_id))))
  | _,_ -> raise (Failure "Attempt to divide incompatible argumnets")



  let ( - ) x y = match x,y with
    Vector xx, Vector yy -> Vector (H.add xx (H.smul yy (C.add_inv C.mul_id)))
  | Operator a, Operator b -> Operator (fun x -> H.add (a x) (H.smul (b x) (C.add_inv C.mul_id)))
  | Number f1, Number f2 -> Number (C.add f1 (C.mul f2 (C.add_inv C.mul_id)))
  | RNumber r1, RNumber r2 -> RNumber (R.add r1 (R.mul r2 R.(add_inv R.mul_id)))
  | RNumber r1, Number f2  -> Number (C.add (C.mk r1 R.add_id) (C.mul f2 (C.add_inv C.mul_id)))
  | Number f1, RNumber r2 -> Number  (C.add f1 (C.mul ((C.mk r2 (R.add_id))) (C.add_inv C.mul_id)))
  | _,_ -> raise (Failure "attempt to subtract incompatible types")


  let int_add x y = x + y

  let ( + ) x y = match x,y with
    Vector xx, Vector yy -> Vector (H.add xx yy)
  | Operator a, Operator b -> Operator (fun x -> H.add (a x) (b x))
  | Number f1, Number f2 -> Number (C.add f1 f2)
  | RNumber r1, RNumber r2 -> RNumber (R.add r1 r2)
  | Number f, RNumber r | RNumber r, Number f -> Number (C.add f (C.mk r R.add_id))
  | _,_                   ->raise (Failure "Attempt to add incompatible types")


  let ( += ) x y = match x with
    Index i -> i := int_add (!i) y
  | _       -> raise (Failure "Attempt to increment non-index value")

  let ( < ) x y = match x,y with
    RNumber r1, RNumber r2 -> R.lt r1 r2
  | Index   i1, Index   i2 -> !i2 >= !i1
  | _,_ -> raise (Failure "Attempt to compare unordered elements")

  let dot u v = match u,v with
    Vector x, Vector y -> Number (H.dot x y)
  | _,_ -> raise (Failure "Attempt to take dot product of non-vectors")

  let norm u = match u with
    Vector x -> RNumber (H.norm x)
  | _ -> raise (Failure "Attempt to take norm of non-vector")


  let abs x = match x with
    Number f -> RNumber (C.abs f)
  |RNumber r -> RNumber (R.abs r)
  |_         -> raise (Failure "Attempt to take absolute value of non-number")

end;;

