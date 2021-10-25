open Float;;

type numb = {
    x1:float;
    y1:float;
    x2:float;
    y2:float;
};;

let mult_float a b = a*.b;;
let div_flaot a b = a/.b;;
let min a b = if a < b then a else b;;
let max a b = if a > b then a else b;;

let rec oper_list li oper = match li with
    | hd::[] -> hd
    | hd::tl -> (oper hd (oper_list tl oper));;
let oper_list_hub x oper = (oper_list [x.x1; x.y1; x.x2; x.y2;] oper);;

let is_zero a = if (a.x1 = a.y1 && a.x1 = 0.) || (a.x2 = a.y2 && a.x2 = 0.) then true else false;;
let nan_detector a b = if(is_nan a) || (is_nan b) then nan else 1.0;;
let degenerate_detector x = if(is_nan (oper_list_hub x nan_detector)) then true else false;;

let oper_el_hub x intervals op oper_for_list = 
    oper_list_hub {
        x1 = (op x intervals.x1);
        y1 = (op x intervals.y1);
        x2 = (op x intervals.x2);
        y2 = (op x intervals.y2);
    } oper_for_list;;

let razy_helper x y inter oper = (oper (oper_el_hub x inter mult_float oper) (oper_el_hub y inter mult_float oper));;
let flip a = {
    x1 = 1./. a.x1;
    y1 = 1./. a.y1;
    x2 = 1./. a.x2;
    y2 = 1./. a.y2;
};;

(*KONSTRUKTORY*)
let wartosc_dokladnosc x p = {
    x1 = x-.x/.100.*.p; 
    y1 = x+.x/.100.*.p; 
    x2 = x-.x/.100.*.p; 
    y2 = x+.x/.100.*.p;
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
let sr_wartosc x = if(degenerate_detector x) then nan else (x.x1+.x.y2)/.2.;;

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

let razy a b = { 
    x1 = (razy_helper a.x1 a.y1 b min);
    y1 = (razy_helper a.x1 a.y1 b max);
    x2 = (razy_helper a.x2 a.y2 b min);
    y2 = (razy_helper a.x2 a.y2 b max);
};;

let podzielic a b = {
    
}