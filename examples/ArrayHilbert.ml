open Array
open Hilbert

module ArrayHilbert : 
  HilbertSpace 
    with type v = float array 
    with type r = float
    with type c = float
  = struct
  type v = float array
  type r = float
  type c = float


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


  let addv v1 v2 = 
    let n = length v1 in
    init n (fun i -> v1.(i) +. v2.(i))
  let subv v1 v2 = 
    let n = length v1 in
    init n (fun i -> v1.(i) -. v2.(i))
  let mulv a v = 
    let n = length v in
    init n (fun i -> v.(i) *. a)

  let divv v a =
    let n = length v in
    init n (fun i -> v.(i) /. a )

  let dot v1 v2 = 
    let v1v2 = map2 mul v1 v2 in
    fold_left add 0.0 v1v2

  let norm v = sqrt (dot v v)
end;;

