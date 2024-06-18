type key = string
type 'a t

(* [create] initializes a new key-value store with the given initial bucket size. *)
val create : int -> 'a t

(*  [insert] key-value pair in the data structure *)
val insert : key -> 'a -> 'a t -> unit

(* [get] a value associated to a input key in the data structure *)
val get : key -> 'a t -> 'a option

(* [update] existing key in the data structure *)
val update : key -> 'a -> 'a t -> unit

(*  [delete] key from the data structure *)
val delete : key -> 'a t -> unit
