open Alcotest

type key = string

type 'a slot = {
  key : key option; [@equal fun a b -> a = b]
  value : 'a option; [@equal fun a b -> a = b]
  deleted : bool; [@equal fun a b -> a = b]
}
[@@deriving eq]

type 'a t = {
  buckets : 'a slot array;
  (* mutable keyword makes sense when insertion or deletion occur in which case
     size increases by one and decreases by same respectively. *)
  size : int;
  locks : Mutex.t array;
}

let equal_slot slot_a slot_b = equal_slot slot_a slot_b

let pp_print_key fmt = function
  | None -> Format.fprintf fmt "None"
  | Some k -> Format.fprintf fmt "Some %s" k

let pp_print_value pp_v fmt = function
  | None -> Format.fprintf fmt "None"
  | Some v -> Format.fprintf fmt "Some %a" pp_v v

let pp_print_slot pp_value fmt slot =
  let { key; value; deleted } = slot in
  Format.fprintf fmt "{ key = %a; value = %a; deleted = %b }" pp_print_key key
    (pp_print_value pp_value) value deleted

let testable_slot pp_value =
  testable (pp_print_slot pp_value) (equal_slot ( = ))

let create initial_bkt_size =
  let buckets =
    Array.make initial_bkt_size { key = None; value = None; deleted = false }
  in
  let locks = Array.make initial_bkt_size (Mutex.create ()) in
  { buckets; size = 0; locks }

let tbl : int t = create 2

let test_create_op () =
  let { buckets; size; locks } = tbl in
  let _size, _locs = (size, locks) in
  let actual = buckets in
  let expected =
    [|
      { key = None; value = None; deleted = false };
      { key = None; value = None; deleted = false };
    |]
  in
  Alcotest.(check (array (testable_slot Format.pp_print_int)))
    "same tbl" actual expected

let () =
  run "Hash_tbl Operations"
    [
      ( "test create operation",
        [ test_case "test create operation" `Quick test_create_op ] );
    ]
