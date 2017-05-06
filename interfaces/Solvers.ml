module type Solver = sig
  (** Type for solver parameters *)
  type s
  (** Type of vectors *)
  type v
  (** Type of real numbers *)
  type r
  (** Type of complex numbers *)
  type c
  (** The solver *)
  val solve : s -> (v->v) -> v -> v
end;;
