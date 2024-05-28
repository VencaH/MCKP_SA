type item = {item_id: int; item_class: int; item_id_in_class: int; item_size: int; item_value: int}

let no_classes = 4 
let size_class = 3
let size_limit = 100


let pow x n =
    let rec inner_pow y x n =
       match y, n with
       | 0, _  -> 0
       | 1, _ -> 1
       | _, 0 -> 1
       | y, 1 -> y
       | y,n -> 
               inner_pow (y*x) x  (n-1) in
    inner_pow x x n

let () =
    print_endline "Pow test:";
    print_int 3;
    print_newline();
    print_int 4;
    print_newline();
    print_int (pow 3 4);
    print_newline()
    


let create_item (id:int): item =
     {
        item_id = id;
        (*
        item_class = (id/size_class) +1;
        item_id_in_class = (id mod (size_class)) +1;
        *)
        item_class = (id/size_class);
        item_id_in_class = id mod size_class;
        item_size = (Random.int 50) +1;
        item_value = (Random.int 50) +1;
    }

let items =
    Array.init (no_classes*size_class) (fun x -> x) 
    |> Array.map create_item

let ()  =
    let print_sums() =
        let sum_size (acc:int) (item:item): int =
            acc + item.item_size in

        let sum_value (acc:int) (item:item): int =
            acc + item.item_value in
        
        Printf.printf "| Sum:                         %4d    %4d |" (Array.fold_left sum_size 0 items) (Array.fold_left sum_value 0 items) in
    
    let print_item (item: item): unit =
        Printf.printf "| %3d |    %3d |          %2d |   %2d |    %2d |" item.item_id item.item_class item.item_id_in_class item.item_size item.item_value;
        print_newline() in
    
    print_endline "|  id |  class | id in class | size | value |";
    print_endline "---------------------------------------------";
    Array.iter print_item items;
    print_endline "---------------------------------------------";
    print_sums();
    print_newline();
    print_endline "Done!"

(* cost function *)

let cost_function (combination: int array) : int =
    let index_cmb = 
        Array.mapi (fun i -> fun x -> (i,x)) combination in
    let get_id (id_with_class: int * int) =
        let (id_class, id_in_class) = id_with_class in
            (id_class)*size_class + (id_in_class) in
    let size =
        let size_acc (acc:(int)) (item_id:(int*int)): int =
            acc + items.(get_id(item_id)).item_size in
        Array.fold_left size_acc 0 index_cmb in        
        let value_acc (acc:int) (item_id:(int*int)) =
            acc + items.(get_id(item_id)).item_value in
    if size > size_limit then 
        -1
    else 
        Array.fold_left value_acc 0 index_cmb 


let () =
    print_endline "Cost for 0 3 8 11 ";
    cost_function [|0; 0; 2; 2|]
    |> print_int;
    print_newline();
    print_endline "Done!"

(* brute force  *)   
let next (previous: int array) =
    let rec inner_next (index:int) (prev:int array) = 
        if index > (no_classes -1) then [||]
        else
        match prev.(index) with
        | 2 ->  Array.set prev index 0;
                 inner_next (index + 1) prev
        | x -> Array.set prev index (x+1);
                prev in
   inner_next 0 previous

type result = {cost:int; items: int array; iters: int}

let eval_all =
    let rec eval_next (iter:int) (current: int array) (best: int array) (best_cost: int):result=
        let current_cost = cost_function current in
        let next_arr = next current in
        match next_arr with
        | [||] -> {
                    cost = best_cost;
                    items = best;
                    iters = iter
                }
        | next_arr ->
            if current_cost > best_cost then
                eval_next (iter+1) next_arr current current_cost
            else eval_next (iter+1) next_arr best best_cost in
    let start = Array.make no_classes 0 in
    eval_next 1 (start) (start) (-1)

let  () = 
    let result = eval_all in
    print_endline "best cost found";
    print_int result.cost;
    print_newline();
    print_endline "no of iters:";
    print_int result.iters
   
(* simulated annealing *)

(* Metropolis  *)

let euler: float = 2.71828 


let metropolis (t: float) (min_t: float) (step_t: float) (best_cost: int) (current_cost:int):(float*int) =
    let diff =  Float.of_int (current_cost - best_cost) in
    if diff > 0. then ((max (t -. step_t) min_t),current_cost)
    else 
        let prob = 1./.(euler ** (diff/.t)) in
            if prob > (Random.float 1.) then ((max (t -. step_t) min_t),current_cost)
            else (t, best_cost)



