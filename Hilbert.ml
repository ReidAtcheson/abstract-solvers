(** An "impure" hilbert space for more efficient implementations *)
module type HilbertSpace_lower = sig
  (** Type of vectors *)
  type v
  (** Underlying field *)
  type f
  (** Underlying real numbers *)
  type r
  (** Make new vector.*)
  val make : unit -> v
  (** Deferred evaluation of zero vector.*)
  val zero : unit -> v
  (** Copy right vector to left vector.*)
  val copy : v -> v -> unit
  (** Swap input vectors.*)
  val swap : v -> v -> unit
  (** scale x a = x <- a*x *)
  val scale : v -> f -> unit
  (** axpy y a x = y <- y + a*x *)
  val axpy : v -> f -> v -> unit
  (** Dot product of two vectors.*)
  val dot : v -> v -> f
  (** Norm of a vector*)
  val norm : v -> r
end;;


(** Pure hilbert space for ease of use *)
module type HilbertSpace = sig
  (** Type of vectors *)
  type v
  (** Type of underlying field *)
  type f
  (** Type of underlying real numbers *)
  type r
  (** Zero vector. *)
  val zero : v
  (** Add two vectors *)
  val add : v -> v -> v
  (** Scalar multiplication *)
  val smul : v -> f -> v

  (** Dot product of two vectors.*)
  val dot : v -> v -> f
  (** Norm of a vector*)
  val norm : v -> r
end;;



