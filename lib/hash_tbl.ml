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

let create initial_bkt_size =
  let buckets =
    Array.make initial_bkt_size { key = None; value = None; deleted = false }
  in
  let locks = Array.make initial_bkt_size (Mutex.create ()) in
  { buckets; size = 0; locks }

let hash key bkt_size =
  let h = Hashtbl.hash key in
  h mod bkt_size

let find_slot key tbl =
  let { buckets; _ } = tbl in
  let bucket_length = Array.length buckets in
  let bucket_index = hash key bucket_length in
  let rec probe i =
    if i < bucket_length then
      (* search for slots starting from most likely bucket index (bucket_index)
         while following a linear probing sequence (bucket_index + 1)*)
      let slot = buckets.((bucket_index + i) mod bucket_length) in
      match slot.key with
      | Some k when k = key -> Some slot
      (* useful when we want to insert or update the slot *)
      | None when not slot.deleted -> Some slot
      | _ -> probe (i + 1)
    else None
  in
  probe 0

let insert key value tbl =
  let bucket_index = hash key (Array.length tbl.buckets) in
  let bucket_lock = tbl.locks.(bucket_index) in
  Mutex.lock bucket_lock;

  (try
     match find_slot key tbl with
     | Some slot when slot.key = None || slot.deleted ->
         slot.key <- Some key;
         slot.value <- Some value;
         (* Is slot.deleted at this point not false? *)
         slot.deleted <- false;
         tbl.size <- tbl.size + 1
     | Some slot -> slot.value <- Some value (* update existing key *)
     | None -> failwith "Didn't find any slot, bucket is full (Hint: resize)!"
   with e ->
     Mutex.unlock bucket_lock;
     raise e);

  Mutex.unlock bucket_lock

let get key tbl =
  let bucket_index = hash key (Array.length tbl.buckets) in
  let bucket_lock = tbl.locks.(bucket_index) in
  Mutex.lock bucket_lock;
  let result =
    match find_slot key tbl with
    | Some slot when slot.key = Some key && not slot.deleted -> slot.value
    | _ -> None
  in
  Mutex.unlock bucket_lock;
  result

let delete key tbl =
  let bucket_index = hash key (Array.length tbl.buckets) in
  let bucket_lock = tbl.locks.(bucket_index) in
  Mutex.lock bucket_lock;

  (try
     match find_slot key tbl with
     | Some slot when slot.key = Some key && slot.deleted ->
         slot.deleted <- true;
         tbl.size <- tbl.size - 1
     | _ -> ()
   with e ->
     Mutex.unlock bucket_lock;
     raise e);

  Mutex.unlock bucket_lock

let update key value tbl =
  let bucket_index = hash key (Array.length tbl.buckets) in
  let bucket_lock = tbl.locks.(bucket_index) in
  Mutex.lock bucket_lock;

  (try
     match find_slot key tbl with
     | Some slot when slot.key = Some key ->
         if slot.deleted then tbl.size <- tbl.size + 1;
         slot.value <- Some value;
         slot.deleted <- false
     | _ ->
         (* insert the key if it doesn't exist *)
         insert key value tbl
   with e ->
     Mutex.unlock bucket_lock;
     raise e);

  Mutex.unlock bucket_lock

(* let resize_bucket tbl =
   let new_bucket : 'a t = create (2 * Array.length tbl.buckets) in
   let rec probe i =
     if i < Array.length tbl.buckets then
       let slot = tbl.buckets.(i) in
       let key = slot.key |> Option.get in
       (* let bucket_index = hash key (Array.length new_bucket.buckets) in
          let slot = tbl.buckets.((bucket_index + 1) mod (Array.length tbl.buckets)) in *)
       match slot.key with
       | Some k when k = key && slot.deleted -> insert key slot.value new_bucket
       | _ -> probe (i + 1)
   in
   probe 0;
   tbl.buckets <- new_bucket.buckets *)

(* else tbl.buckets <- new_bucket *)
