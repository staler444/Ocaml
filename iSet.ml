(* Bartek Kucypera

oznaczenia 
x : pojednynczy punkt (int)
(a,b), p, k: przedziały (int*int)
tr: drzewo (t)
l, r: lewe, prawe poddrzewo (t)
*)

(*l, k, r, wysokosc: h, ilosc elementów: s*)
type t = 
    | Empty 
    | Node of t * (int * int) * t * int * int

let empty = Empty

(*anti over flow sum *)
let aof a b = if ((a >= 0) && (b >= 0) && (a >= max_int - b)) then max_int
    else if ((a < 0) && (b < 0)) && ((min_int - a) >= b) then min_int
    else a+b
    
(*anti over flow odległość*)
let odl a b = if (a = min_int) && (b >= -1) then max_int else if a = min_int then abs(min_int+(abs b)) else aof b (-a)  

let is_empty = function
    | Empty -> true
    | _ -> false

(*zwraca wyskość *)
let height = function
    | Node(_, _, _, h, _) -> h
    | Empty -> 0

(*zwraca ilośc elementów w poddrzewie raze m z elementami w wierzchołku*)
let sumSub = function 
    | Node(_, _, _, _, s) -> s
    | Empty -> 0

(*zwraca ilośće elementów danego noda *)
let sumNode = function
    | (a, b) -> (aof (odl a b) 1)

(*sprawdza czy przedział x przecina się z nod*)
let in_Node x nod = match nod with 
    | Node(_, (a, b), _, _, _) -> (a <= x && x <= b)
    | Empty -> false

(*sprawdza czy (a,b) przecina się z nodem tr albo czy przylega do niego*)
let is_mtf (a, b) tr = match tr with
	| Empty -> false
	| Node(l, (k1, k2), r, _, _) -> ((((aof b 1) = k1) || ((aof a (-1)) = k2)) || (((k1 <= a && a <= k2 ) || ( k1 <= b && b <= k2)) || (a <= k1 && k2 <= b)))

let rec mem x tr = match tr with
    | Empty -> false
    | Node (l, (a, b), r, _, _) -> if (in_Node x tr) then true else
        if x < a then (mem x l) else (mem x r)

let make l (a, b) r = Node(l, (a, b), r, ((max (height l) (height r))+1),
    aof (aof (sumSub l) (sumSub r)) (sumNode (a, b))) 

let rec iter f tr = match tr with 
    | Empty -> ()
    | Node(l, k, r, _, _) -> iter f l; f k; iter f r

let rec fold f tr acc = match tr with 
    | Empty -> acc
    | Node (l, k, r, _, _) -> fold f r (f k (fold f l acc))

let elements trr =
    let rec pom tr res = match tr with 
        | Empty -> res 
        | Node (l, k, r, _, _) -> pom l (k::(pom r res))  
    in pom trr []

(*zwraca ile jest elementów <= x*)
let rec below x tr = match tr with 
    | Empty -> 0
    | Node(l, (a, b), r, _, _) -> 
        if (in_Node x tr) then aof (sumSub l) (sumNode (a, x))
            else if x < a then below x l 
                else aof (aof (below x r) (sumNode (a, b))) (sumSub l)

(*przepisane balansowanie z pSet*)
let bal l k r =
    let hl = height l in
    let hr = height r in
    if hl > hr + 2 then
	match l with
	    | Node (ll, lk, lr, _, _) ->
		if height ll >= height lr then make ll lk (make lr k r)
		else
		(match lr with
		    | Node (lrl, lrk, lrr, _, _) ->
			make (make ll lk lrl) lrk (make lrr k r)
		    | Empty -> assert false)
	    | Empty -> assert false
    else if hr > hl + 2 then
	match r with
	    | Node (rl, rk, rr, _, _) ->
		if height rr >= height rl then make (make l k rl) rk rr
		else
		(match rl with
		    | Node (rll, rlk, rlr, _, _) ->
			make (make l k rll) rlk (make rlr rk rr)
		    | Empty -> assert false)
	    | Empty -> assert false
	else make l k r

(*dodajemy pojednyńczy przedział do drzewa, zakłada że przedział nie przylega i nie ma przeciencia z żadnym innym z drzewa *)
let rec insert p tr = match p, tr with 
    | _, Empty -> make Empty p Empty
    | (a, b), Node(l, (k1, k2), r, _, _) -> 
        if b < k1 then bal (insert p l) (k1, k2) r
        else bal l (k1, k2) (insert p r)

(*przecina noda w punkcie x, zwraca: (lewe drzewo po przecięciu, czy x był w nodzie, prawe drzewo po przecięciu*)
let cutNode x tr = match tr with
    | Node(l, (a, b), r, h, _) -> 
        ((if x = a then l else insert (a, x-1) l), true,
        (if x = b then r else insert (x+1, b) r))
    | Empty -> (Empty, false, Empty)

(*mergowanie 2 drzew i jednego noda*)
let rec merge l k r = match l, r with
    | Empty, tr | tr, Empty -> insert k tr
    | Node(ll, kl, rl, hl, _), Node(lr, kr, rr, hr, _) ->
        if hl > (hr + 2) then bal ll kl (merge rl k r)
        else if hr > (hl + 2) then bal (merge l k lr) kr rr
        else make l k r

(*mergowanie 2 drzew*)
let rec merge2 l r = match l, r with
    | Empty, tr | tr, Empty -> tr
    | Node(ll, kl, rl, hl, _), Node(lr, kr, rr, hr, _) -> 
        if hl > (hr + 2) then bal ll kl (merge2 rl r)
        else bal (merge2 l lr) kr rr 

let rec split x tr = match tr with
    | Empty -> (Empty, false, Empty)
    | Node(l, (a, b), r, _, _) -> 
        if in_Node x tr then cutNode x tr
        else if x < a then match (split x l) with 
			| (ls, bol,rs) -> (ls, bol, merge rs (a, b) r)
        else match (split x r) with 
			| (ls, bol, rs) -> ((merge l (a, b) ls), bol, rs)

let rec remove (a, b) tr = match (split a tr), (split b tr) with
    | (l, _, _), (_, _, r) -> merge2 l r

(*zwraca sume p i wszystkich przedziałów o niezerowym przecięciu lub przylegających z p*)
let rec find p tr = match p, tr with
| (a, b), Empty -> p
| (a, b), Node(l, (ak, bk), r, _, _) -> let zjb = is_mtf p tr in  
	if zjb then let (lew, _) = find p l and (_, praw) = find p r in (min (min a ak) lew, max (max b bk) praw)
	else if b < ak then find p l else find p r  

(*usuwamy wszystkie elementy o niepustym przecięciu z p, wstawiamy sume p i wszystkich tych przedziałów co je usuneliśmy*)
let add p tr = let pom = find p tr in let newtr = remove pom tr in (insert pom newtr);;
