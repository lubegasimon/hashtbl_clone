open Alcotest
open Hash_tbl

let equal_tbl (tbl_a : 'a t) (tbl_b : 'a t) =
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

let () =
  run "Hash_tbl Operations"
    [
      ( "test create operation",
        [ test_case "test create operation" `Quick test_create_op ] );
    ]
