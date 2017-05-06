module type Solver = sig
  (** Type for solver parameters *)
  type s
  (** Type of vectors *)
  type v
  (** The solver *)
  val solve : s -> (v->v) -> v -> v
end;;
