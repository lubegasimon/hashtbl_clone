type key = string

type 'a slot = {
  mutable key : key option;
  mutable value : 'a option;
  mutable deleted : bool;
}

type 'a t = {
  buckets : 'a slot array;
  (* mutable keyword makes sense when insertion or deletion occur in which case
     size increases by one and decreases by same respectively. *)
  mutable size : int;
  locks : Mutex.t array;
}

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
