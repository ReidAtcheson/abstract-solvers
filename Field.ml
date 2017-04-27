(** A module to abstract away real number type*)
module type OrderedField = sig
  (** "abstract" real number type *)
  type r
  (** lt x y = true if x<y else false *)
  val lt : r -> r -> bool


  (** Add two field elements *)
  val add : r -> r -> r
  (** Additive identity *)
  val add_id : r
  (** Additive inverse *)
  val add_inv : r -> r

  (** Multiply two field elements *)
  val mul : r -> r -> r
  (** Multiplicative identity *)
  val mul_id : r
  (** Multiplicative inverse *)
  val mul_inv : r -> r


  (** Take absolute value of field element *)
  val abs : r -> r
  (** Compute square root of field element *)
  val sqrt : r -> r
end;;


(** A module to asbtract away complex number type *)
module type TopologicalField = sig
  (** "abstract" real number type *)
  type r
  (** "abstract" complex number type *)
  type c
  (** Real part *)
  val re : c -> r
  (** Imaginary part *)
  val im : c -> r
  (** Make number from real and imaginary part *)
  val mk : r -> r -> c
  (* Complex conjugate *)
  val conj : c -> c

  (** Add two field elements *)
  val add : c -> c -> c
  (** Additive identity *)
  val add_id : c
  (** Additive inverse *)
  val add_inv : c -> c

  (** Multiply two field elements *)
  val mul : c -> c -> c
  (** Multiplicative identity *)
  val mul_id : c
  (** Multiplicative inverse *)
  val mul_inv : c -> c


  (** Take absolute value of field element *)
  val abs : c -> r
  (** Compute square root of field element *)
  val sqrt : c -> c

end;;
