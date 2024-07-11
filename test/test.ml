open Alcotest
open Hash_tbl

let equal_slot slot_a slot_b =
  match (slot_a, slot_b) with
  | ( { key = key_a; value = val_a; deleted = deleted_a },
      { key = key_b; value = val_b; deleted = delete_b } ) ->
      key_a = key_b && val_a = val_b && deleted_a = delete_b

let equal_tbl tbl_a tbl_b =
  match (tbl_a, tbl_b) with
  | ( { buckets = buckets_a; size = size_a; _ },
      { buckets = buckets_b; size = size_b; _ } ) ->
      buckets_a = buckets_b && size_a = size_b

let pp_print_key fmt = function
  | None -> Format.fprintf fmt "None"
  | Some k -> Format.fprintf fmt "Some %s" k

let pp_print_value pp_v fmt = function
  | None -> Format.fprintf fmt "None"
  | Some v -> Format.fprintf fmt "Some %a" pp_v v

let pp_print_slot pp_v fmt slot =
  let { key; value; deleted } = slot in
  Format.fprintf fmt "{ key = %a; value = %a; deleted = %b }" pp_print_key key
    (pp_print_value pp_v) value deleted

let pp_print_array pp_elem fmt arr =
  Format.fprintf fmt "[|";
  Array.iteri
    (fun i e -> if i > 0 then Format.fprintf fmt "; " else pp_elem fmt e)
    arr;
  Format.fprintf fmt "|]"

let pp_print_slots pp_v fmt (slots : 'a slot array) =
  pp_print_array (pp_print_slot pp_v) fmt slots

let pp_print_locks fmt (locks : Mutex.t array) =
  Format.fprintf fmt "[|";
  Array.iter (fun _ -> Format.fprintf fmt "") locks;
  Format.fprintf fmt "|]"

let pp_print_tbl pp_v fmt tbl =
  let { buckets; size; locks } = tbl in
  Format.fprintf fmt "{ buckets = %a; size = %i; locks = %a}"
    (pp_print_slots pp_v) buckets size pp_print_locks locks

let testable_slot pp_value = testable (pp_print_slot pp_value) equal_slot
let testable_table pp_v = testable (pp_print_tbl pp_v) equal_tbl
let tbl = create 2

let test_create_op () =
  let actual = tbl in
  let expected =
    {
      buckets =
        [|
          { key = None; value = None; deleted = false };
          { key = None; value = None; deleted = false };
        |];
      size = 0;
      locks = [||];
    }
  in
  Alcotest.(check (testable_table Format.pp_print_int))
    "same table" actual expected

(* FIXME: BUG: insert should insert in only one empty slot, not all of them *)
let test_insert_op () =
  let () = insert "first_key" 1 tbl in
  let actual = tbl in
  let expected =
    {
      buckets =
        [|
          { key = Some "first_key"; value = Some 1; deleted = false };
          { key = Some "first_key"; value = Some 1; deleted = false };
        |];
      size = 1;
      locks = [||];
    }
  in
  Alcotest.(check (testable_table Format.pp_print_int))
    "same table" actual expected

let test_find_slot_op () =
  let slot = find_slot "first_key" tbl in
  let actual =
    match slot with
    | Some slot -> slot
    | None -> failwith "Did not find any slot!"
  in
  let expected = { key = Some "first_key"; value = Some 1; deleted = false } in

  Alcotest.(check (testable_slot Format.pp_print_int))
    "same slot" actual expected

let () =
  run "Hash_tbl Operations"
    [
      ( "test create operation",
        [ test_case "test create operation" `Quick test_create_op ] );
      ( "test insert operation",
        [ test_case "test insert operation" `Quick test_insert_op ] );
      ( "test find_slot operation",
        [ test_case "test find_slot operation" `Quick test_find_slot_op ] );
    ]
