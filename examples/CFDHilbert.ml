open Array
open Hilbert


open Core
open Ctypes
open PosixTypes
open Foreign

type cvector = { ptr_ : unit Ctypes_static.ptr }
let make_ = foreign "make" (int64_t @-> int64_t @-> int64_t @-> returning (ptr void))
let unmake_ = foreign "unmake" ((ptr void) @-> returning void)
let fill_ = foreign "fill" ( (ptr void) @-> double @-> returning void)
let addv_ = foreign "addv" ( (ptr void) @-> (ptr void) @-> (ptr void) @-> returning void)
let subv_ = foreign "subv" ( (ptr void) @-> (ptr void) @-> (ptr void) @-> returning void)
let mulv_ = foreign "mulv" ( (ptr void) @-> double @-> (ptr void) @-> returning void)
let divv_ = foreign "divv" ( (ptr void) @-> double @-> (ptr void) @-> returning void)
let dot_ = foreign "dot" ( (ptr void)  @-> (ptr void) @-> returning double)
let op_   = foreign "fd"  ( (ptr void)  @-> (ptr void) @-> returning void)

let nx = Signed.Int64.of_int 64
let ny = Signed.Int64.of_int 64
let nz = Signed.Int64.of_int 64

let unmake x = unmake_ (x.ptr_)
let make nx ny nz = 
  let y = {ptr_ = (make_ nx ny nz) } in
  Gc.finalise unmake y;
  y


let ones () =
  let x = make nx ny nz in
  fill_ x.ptr_ 1.0;
  x

let op x = 
  let y = make nx ny nz in
  let () = op_ x.ptr_ y.ptr_ in
  y



module CFDHilbert : 
  HilbertSpace
    with type v = cvector
    with type r = float
    with type c = float
  = struct
  type v = cvector
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
    let v = make nx ny nz in
    let () = addv_ v1.ptr_ v2.ptr_ v.ptr_ in
    v

  let subv v1 v2 = 
    let v = make nx ny nz in
    subv_ v1.ptr_ v2.ptr_ v.ptr_;
    v



  let mulv a v   =
    let w = make nx ny nz in
    mulv_ v.ptr_ a w.ptr_;
    w
    
  let divv v a   =
    let w = make nx ny nz in
    divv_ v.ptr_ a w.ptr_;
    w
 
  let dot v1 v2 = (dot_ v1.ptr_ v2.ptr_)
 

  let norm v = sqrt (dot v v)
end;;

