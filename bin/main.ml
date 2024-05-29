open Base

let () = Random.self_init ()

let time_it f =
  let t = Stdlib.Sys.time () in
  let result = f () in
  let end_t = Stdlib.Sys.time () in
  (end_t -. t, result)

type item =
  { item_id: int
  ; item_class: int
  ; item_id_in_class: int
  ; item_size: int
  ; item_value: int }

let no_classes = 15

let size_class = 3

let size_limit = 300

let pow x n =
  let rec inner_pow y x n =
    match (y, n) with
    | 0, _ ->
        0
    | 1, _ ->
        1
    | _, 0 ->
        1
    | y, 1 ->
        y
    | y, n ->
        inner_pow (y * x) x (n - 1)
  in
  inner_pow x x n

let create_item (id : int) : item =
  { item_id= id
  ; (*
        item_class = (id/size_class) +1;
        item_id_in_class = (id mod (size_class)) +1;
        *)
    item_class= id / size_class
  ; item_id_in_class= Stdlib.( mod ) id size_class
  ; item_size= Random.int 50 + 1
  ; item_value= Random.int 50 + 1 }

let items =
  List.init (no_classes * size_class) ~f:(fun x -> x) |> List.map ~f:create_item

(* print functions  *)
let get_id (id_with_class : int * int) =
  let id_class, id_in_class = id_with_class in
  (id_class * size_class) + id_in_class

let print_values (items : item list) =
  let print_sums () =
    let sum_size (acc : int) (item : item) : int = acc + item.item_size in
    let sum_value (acc : int) (item : item) : int = acc + item.item_value in
    Stdlib.Printf.printf "| Sum:                         %4d    %4d |"
      (List.fold items ~init:0 ~f:sum_size)
      (List.fold_left items ~init:0 ~f:sum_value)
  in
  let print_item (item : item) : unit =
    Stdlib.Printf.printf "| %3d |    %3d |          %2d |   %2d |    %2d |"
      item.item_id item.item_class item.item_id_in_class item.item_size
      item.item_value ;
    Stdlib.print_newline ()
  in
  Stdlib.print_endline "|  id |  class | id in class | size | value |" ;
  Stdlib.print_endline "---------------------------------------------" ;
  List.iter items ~f:print_item ;
  Stdlib.print_endline "---------------------------------------------" ;
  print_sums () ;
  Stdlib.print_newline () ;
  Stdlib.print_newline ()

let () = print_values items

let print_results (result_items : int list) =
  let result_ids = List.mapi result_items ~f:(fun i x -> get_id (i, x)) in
  List.filter items ~f:(fun item ->
      List.exists result_ids ~f:(fun e -> e = item.item_id) )
  |> print_values

(* cost function *)

let cost_function (combination : int list) : int =
  let index_cmb = List.mapi combination ~f:(fun i x -> (i, x)) in
  let size =
    let size_acc (acc : int) (item_id : int * int) : int =
      (List.nth_exn items (get_id item_id)).item_size |> ( + ) acc
    in
    List.fold index_cmb ~init:0 ~f:size_acc
  in
  let value_acc (acc : int) (item_id : int * int) =
    (List.nth_exn items (get_id item_id)).item_value |> ( + ) acc
  in
  if size > size_limit then -1 else List.fold index_cmb ~init:0 ~f:value_acc

(* brute force  *)
let next (previous : int list) : int list =
  let rec inner_next (index : int) (prev : int list) : int list =
    let max = size_class - 1 in
    if index > no_classes - 1 then []
    else
      match List.nth_exn prev index with
      | x when x = max ->
          List.mapi prev ~f:(fun i x -> if i = index then 0 else x)
          |> inner_next (index + 1)
      | _ ->
          List.mapi prev ~f:(fun i x -> if i = index then x + 1 else x)
  in
  inner_next 0 previous

type result = {cost: int; items: int list; iters: int}

let eval_all() =
  let rec eval_next (iter : int) (current : int list) (best : int list)
      (best_cost : int) : result =
    let current_cost = cost_function current in
    let next_arr = next current in
    match next_arr with
    | [] ->
        {cost= best_cost; items= best; iters= iter}
    | next_arr ->
        if current_cost > best_cost then
          eval_next (iter + 1) next_arr current current_cost
        else eval_next (iter + 1) next_arr best best_cost
  in
  let start = Stdlib.List.init no_classes (fun _ -> 0) in
  eval_next 1 start start (-1)

let () =
  let time, result = time_it (eval_all) in
  Stdlib.print_endline "Done bruteforce." ;
  Stdlib.print_string "time: " ;
  Stdlib.print_float time ;
  Stdlib.print_endline "s" ;
  Stdlib.print_endline "result:" ;
  print_results result.items

(* simulated annealing *)

(* Metropolis  *)

let euler : float = 2.71828

let metropolis (t : float) (min_t : float) (step_t : float) (best : int list)
    (best_cost : int) (current : int list) (current_cost : int) :
    float * int * int list =
  let diff = Float.of_int (current_cost - best_cost) in
  if Float.( > ) diff 0. then
    (Float.max (t *. step_t) min_t, current_cost, current)
  else
    let prob = 1. /. Float.( ** ) euler (diff /. t) in
    if Float.( > ) prob (Random.float 1.) then
      (Float.max (t *. step_t) min_t, current_cost, current)
    else (t, best_cost, best)

(* step in local space *)
let local_next (previous : int list) =
  let inner_local_next (class_id : int) (step : int) (prev : int list) =
    let max = size_class - 1 in
    match List.nth_exn prev class_id + step with
    | x when x > max ->
        List.mapi prev ~f:(fun i x -> if i = class_id then 0 else x)
    | x when x < 0 ->
        List.mapi prev ~f:(fun i x -> if i = class_id then max else x)
    | _ ->
        List.mapi prev ~f:(fun i x -> if i = class_id then x + step else x)
  in
  inner_local_next
    (Random.int (no_classes - 1))
    (-1 + (Random.int 1 * 2))
    previous

let eval_local (size : int) (start : int list) (t : float) (min_t : float)
    (step_t : float) : float * int * int list =
  let rec inner_eval_local (size : int) (current : int list) (t : float)
      (min_t : float) (step_t : float) (best : int list) (best_cost : int) :
      float * int * int list =
    match size with
    | 0 ->
        (t, best_cost, best)
    | x ->
        let current_cost = cost_function current in
        let t, best_cost, best =
          metropolis t min_t step_t best best_cost current current_cost
        in
        inner_eval_local (x - 1) (local_next current) t min_t step_t best
          best_cost
  in
  inner_eval_local size start t min_t step_t start (-1)

let simulated_annealing (size : int) (local_size : int) (max_t : float)
    (min_t : float) (step_t : float) =
  let rec inner_sa (size : int) (local_size : int) (iter : int) (t : float)
      (min_t : float) (step_t : float) (current : int list) (best : int list)
      (best_cost : int) : int * int list =
    match size with
    | 0 ->
        (best_cost, best)
    | x ->
        let local_size = min size local_size in
        let t, local_best_cost, current =
          eval_local local_size current t min_t step_t
        in
        let iter = iter + local_size in
        let local_sa =
          inner_sa (x - local_size) local_size iter t min_t step_t current
        in
        if local_best_cost > best_cost then local_sa current local_best_cost
        else local_sa best best_cost
  in
  let current =
    List.init no_classes ~f:(fun _ -> Random.int (size_class - 1))
  in
  inner_sa size local_size 1 max_t min_t step_t current current
    (cost_function current)

let rec best_of (n : int) (current_best : int * int list) =
  match n with
  | 0 ->
      current_best
  | x ->
      let cost, items = simulated_annealing 500000 10 100. 0.1 1. in
      let best_cost, _ = current_best in
      let next_run = best_of (x - 1) in
      if cost > best_cost then next_run (cost, items) else next_run current_best

let () =
  let time, (_, items) =
    time_it (fun () -> simulated_annealing 500000 10 100. 0.1 1.)
  in
  Stdlib.print_endline "Done SA." ;
  Stdlib.print_string "time: " ;
  Stdlib.print_float time ;
  Stdlib.print_endline "s" ;
  Stdlib.print_endline "results: " ;
  print_results items

let avg_of_n n =
  List.fold (List.init n ~f:(fun _ -> 0)) ~init:0 ~f: (fun acc _ -> let (cost, _) =  simulated_annealing 500000 10 100. 0.1 1. in acc + cost)
 (* |> fun x -> x // n *)

let () =
  let avg = avg_of_n 100 in
  Stdlib.print_endline "Average of 100";
  Stdlib.print_int avg;
  Stdlib.print_newline();
