open Field


(** Create a trivial topological field from an ordered field *)
module OrderedToTopological (R : OrderedField)  : TopologicalField with type r = R.r with type c = R.r = struct
  type r = R.r
  type c = R.r
  let re x = x
  let im x = x
  let mk x y = x
  let conj x = x
  let add x y = R.add x y
  let add_id = R.add_id
  let add_inv = R.add_inv
  let mul x y = R.mul x y
  let mul_id = R.mul_id
  let mul_inv = R.mul_inv
  let abs = R.abs
  let sqrt = R.sqrt
end;;
