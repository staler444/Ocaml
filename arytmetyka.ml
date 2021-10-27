open Float;;

type wartosc = {
    x1:float;
    y1:float;
    x2:float;
    y2:float;
};;

let adjust a = if(a.x1 > a.x2) then {x1=a.x2; y1=a.y2; x2=a.x1;y2=a.y2;} else a;;

let mult_float a b = if(a = 0. || b = 0.) then 0. else a*.b;; 
let div_flaot a b = a/.b;;
let min a b = if a < b then a else b;;
let max a b = if a > b then a else b;;
let exclusive_oper a b oper neutral_element = 
    if(is_integer a) && (is_integer b) then oper a b 
    else if (is_integer a) then a
    else if (is_integer b) then b else neutral_element;;
let exclusive_min a b = exclusive_oper a b min infinity;;
let exclusive_max a b = exclusive_oper a b max neg_infinity;;

let from_to x y = {x1 = x;y1 = y;x2 = x;y2 = y;};;

let rec oper_list li oper = match li with
    | hd::[] -> hd
    | hd::tl -> (oper hd (oper_list tl oper));;
let oper_list_hub x oper = (oper_list [x.x1; x.y1; x.x2; x.y2;] oper);;

let only_inf_detector a = ((((is_infinite a.x1) && (is_infinite a.y1)) && (is_infinite a.x2)) && (is_infinite a.y2));;

let is_zero a = if (a.x1 = a.y1 && a.x1 = 0.) || (a.x2 = a.y2 && a.x2 = 0.) then true else false;;
let oper_or oper a b = if(oper a) || (oper b) then nan else 1.0;;
let oper_and oper a b = if(oper a) && (oper b) then nan else 1.0;;

let degenerate_detector x oper_for_list oper_and_or = if(is_nan (oper_list_hub x (oper_and_or oper_for_list))) then true else false;;

let is_two_infinitys a = ((is_infinite a.x1) && (is_infinite a.y2));;

let razy_list_hub a inter oper = oper (mult_float a (oper_list_hub inter max)) (mult_float a (oper_list_hub inter min));;
let razy_helper x y inter oper = oper (razy_list_hub x inter oper) (razy_list_hub y inter oper);;

let razy_standard a b = {
    x1 = (razy_helper a.x1 a.y1 b min);
    y1 = (razy_helper a.x1 a.y1 b max);
    x2 = (razy_helper a.x2 a.y2 b min);
    y2 = (razy_helper a.x2 a.y2 b max);
};;

let razy_two_infinitys_pom x b oper = oper (x*.(oper_list_hub b exclusive_max)) (x*.(oper_list_hub b exclusive_min));;

let mine a b c d = (min (a*.b) (c*.d));;
let maxe a b c d = (max (a*.b) (c*.d));;

(* NIE CZYTAĆ *)
let dif_el_dif_ab el a b = {x1 = neg_infinity; y1 = (maxe el.y1 b el.x2 a); x2 = (mine el.x2 b el.y1 a); y2 = infinity;};;
let dif_el_same_ab el a b = if a < 0. 
    then {x1 = neg_infinity; y1 = (maxe el.x2 a el.x2 b); x2 = (mine el.y1 a el.y1 b); y2 = infinity;}
    else {x1 = neg_infinity; y1 = (maxe el.y1 a el.y1 b); x2 = (mine el.x2 a el.x2 b); y2 = infinity;};; 
let difrent_el el a b = if((a*.b) < 0.) 
    then dif_el_dif_ab el (min a b) (max a b)
    else dif_el_same_ab el (min a b) (max a b);;

let same_el_dif_ab el a b = {x1 = neg_infinity; y1 = 0.; x2 = 0.; y2 = infinity;};;
let same_el_same_ab el a b = if a < 0. 
    then {x1 = neg_infinity; y1 = (maxe el.x2 a el.x2 b); x2 = (mine el.y1 a el.y1 b); y2 = infinity;}
    else {x1 = neg_infinity; y1 = (maxe el.y1 a el.y1 b); x2 = (mine el.x2 a el.x2 b); y2 = infinity;};;

let same_el el a b = if (a*.b < 0.) then same_el_dif_ab el (min a b) (max a b)
else same_el_same_ab el (min a b) (max a b);;

let rozw el a b = if (a = 0.) || (b = 0.) then (from_to neg_infinity infinity)
    else if el.y1 < 0. && el.x2 > 0. then difrent_el el a b
    else same_el el a b;;

let razy_two_infinitys a b = 
    if(is_integer b.y1) && (is_integer b.x2) then rozw a b.x1 b.x2 
    else if (is_integer b.y1) && (is_integer b.y2) then rozw a b.y1 b.y2
    else rozw a b.x1 b.x2;;
(*   Tu już można   *)

let flip_interval_standard_hub (a, b) (c, d) = {x1=a; y1=b; x2=c; y2=d;};;
let flip_interval_standard a b = 
    if (a = 0.) && (b = 0.) then (nan, nan)
    else if (b = 0.) then (neg_infinity, (1./.a))
    else if (a = 0.) then ((1./.b), infinity)
    else ((1./.b), (1./. a));;

let flip_interval_infinite a =
    if (only_inf_detector a) then a
    else if (a.x1 = neg_infinity && a.y2 = infinity) then {x1=infinity; y1=(1./.a.x2); x2=(1./.a.y1); y2=neg_infinity;}
    else if (a.x1 = neg_infinity) then (from_to (1./.a.y1) 0.)
    else (from_to 0. (1./.a.x1));;

let flip_divide a = {x1=neg_infinity; y1=(1./.a.x1); x2=(1./.a.y1); y2=infinity;};;

(*KONSTRUKTORY*)
let wartosc_dokladnosc el p = let fp x oper = oper (x-.x/.100.*.p) ( x+.x/.100.*.p) in {
    x1 = fp el min; 
    y1 = fp el max;
    x2 = fp el min; 
    y2 = fp el max;
};;

let wartosc_od_do x y = {
    x1 = x;
    y1 = y;
    x2 = x;
    y2 = y;
};;

let wartosc_dokladna x = {x1 = x; y1 = x; x2 = x; y2 = x;};;

(*SELEKTORY*)
let in_wartosc x y = if(x.x1 <= y && y <= x.y1) || (x.x2 <= y && y <= x.y2) then true else false;;
let min_wartosc x = (oper_list_hub x min);;
let max_wartosc x = (oper_list_hub x max);;
let sr_wartosc x = if(degenerate_detector x is_nan oper_or) then nan else (x.x1+.x.y2)/.2.;;

(*MODYFIKATORY*)
let plus a b = {
    x1 = a.x1 +. (oper_list_hub b min);
    y1 = a.y1 +. (oper_list_hub b max);
    x2 = a.x2 +. (oper_list_hub b min);
    y2 = a.y2 +. (oper_list_hub b max);
};;

let minus a b = {
    x1 = a.x1 -. (oper_list_hub b max);
    y1 = a.y1 -. (oper_list_hub b min);
    x2 = a.x2 -. (oper_list_hub b max);
    y2 = a.y2 -. (oper_list_hub b min);
};;

let razy a b = 
    if(only_inf_detector a) || (only_inf_detector b) then (from_to infinity neg_infinity)
    else if (is_zero a) || (is_zero b) then (wartosc_dokladna 0.)
    else if (is_two_infinitys a) then razy_two_infinitys a b
    else if (is_two_infinitys b) then razy_two_infinitys b a
    else razy_standard a b;;

let podzielic a b = 
    if (degenerate_detector b is_nan oper_or) || (is_zero b) then (wartosc_dokladna nan)
    else if (degenerate_detector b is_infinite oper_or) then razy (flip_interval_infinite b) a
    else if (b.x1 < 0. && b.y1 > 0.) then razy a (flip_divide b)
    else razy a (flip_interval_standard_hub (flip_interval_standard b.x1 b.y1) (flip_interval_standard b.x2 b.y2));;