module type HilbertSpace = sig
  (** Type of vectors *)
  type v
  (** Type of ordered field (probably Real numbers) *)
  type r
  (** Type of Complex numbers (maybe abstractable to topological field?) *)
  type c


  (** Comparisons with real numbers *)
  val lt : r -> r -> bool
  (** Convenience function for outputs *)
  val string_of_r : r -> string

  (** Operations on complex numbers *)

  (** Convenience function to turn floats into complex numbers *)
  val mkf : float -> float -> c
  (** Embed real number into complex number *)
  val emb : r -> c
  (** Make complex numbers from real and imaginary parts *)
  val mk  : r -> r -> c
  (** Take real part of complex number *)
  val re  : c -> r
  (** Take imaginary part of complex number *)
  val im  : c -> r
  (** Complex multiplication *)
  val mul : c -> c -> c
  (** Complex addition *)
  val add : c -> c -> c
  (** Complex subtraction *)
  val sub : c -> c -> c
  (** Complex division *)
  val div : c -> c -> c
  (** Get absolute value of complex number *)
  val abs : c -> r
  (** Take square root of complex number *)
  val sqrt : c -> c


  (** Note: to do arithmetic with reals, just embed them
   * into complex field*)


  (** Operations on vectors *)

  (** Vector addition *)
  val addv : v -> v -> v
  (** Vector subtraction *)
  val subv : v -> v -> v
  (** Scalar multiplication *)
  val mulv : c -> v -> v
  (** Scalar division *)
  val divv : v -> c -> v
  (** Dot product *)
  val dot  : v -> v -> c
  (** Norm *)
  val norm : v -> r
end;;



module DSL (H : HilbertSpace) = struct
  type v = H.v
  type r = H.r
  type c = H.c
  let ( < ) = H.lt
  let string_of_r = H.string_of_r

  let mkf = H.mkf
  let mk = H.mk
  let emb = H.emb
  let re = H.re
  let im = H.im
  let ( *. ) = H.mul
  let ( +. ) = H.add
  let ( -. ) = H.sub
  let ( /. ) = H.div
  let abs = H.abs
  let sqrt = H.sqrt

  let ( *.. ) x y = re ((emb x) *. (emb y))
  let ( +.. ) x y = re ((emb x) +. (emb y))
  let ( /.. ) x y = re ((emb x) /. (emb y))
  let ( -.. ) x y = re ((emb x) -. (emb y))


  (** Operations on vectors *)

  let ( + ) = H.addv
  let ( - ) = H.subv
  let ( * ) = H.mulv
  let ( / ) = H.divv
  let dot = H.dot
  let norm = H.norm

end;;
