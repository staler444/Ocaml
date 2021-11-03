type 'a queue = | Node of 'a queue * 'a queue * 'a * int | Leaf;;

exception Empty;; 
let empty = Leaf;;
let make_queue a = Node(Leaf, Leaf, a, 0);;
let get_npl a = match a with 
    | Leaf -> 0
    | Node (_, _, _, npl) -> npl;;

let rec join a b = match a, b with 
    | (Leaf, tr) | (tr, Leaf) -> tr
    | Node (l1, r1, val1, npl1), Node(l2, r2, val2, npl2) -> if(val1 <= val2)
    then let new_r = join r1 b in if((get_npl l1) >= (get_npl new_r)) 
        then Node(l1, new_r, val1, (get_npl new_r)+1)
        else Node(new_r, l1, val1, (get_npl l1)+1)
    else (join b a);;

let add b q = join q (make_queue b);;
let delete_min q = match q with
    | Node(l, r, el, npl) -> (el, (join l r))
    | Leaf -> raise Empty;;
let is_empty a = match a with 
    | Leaf -> true
    | _ -> false;;