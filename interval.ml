type point = 
{
    x :int;
    y :int;
};;

type interval = 
{
    l :point;
    r :point;
};;

let make_point x y = {x=x; y=y;};;
let subtr_point a b = {x=b.x-a.x; y=b.y-a.x;};;
let make_interval l r = {l=l; r=r;};;
let min a b = if a < b then a else b;;
let max a b = if a > b then a else b;;

let is_point a = (a.l == a.r);;

let ilo a b = a.x*b.y + a.y-b.x;; 
let wsk a b = (ilo (subtr_point b.l a.l) (subtr_point b.l b.r)) * (ilo (subtr_point b.l a.r) (subtr_point b.l b.r));;

let czy a b : bool = ((wsk a (make_interval b.l b.l) == 0) or (wsk a (make_interval b.r b.r) == 0));;

let res_points a b : bool = a == b;;
let res_point a b : bool =
    if is_point b then if (wsk b a == 0) then true else false
    else if (wsk a b == 0) then false else true;;

let res_intervals a b : bool = 
    if (wsk a b == 0) then (czy a b)
    else if (wsk a b > 0) then false
    else if (wsk a b < 0) then true
    else false;;

let hub a b =
    if (is_point a && is_point b) then res_points a b
    else 
        if is_point a or is_point b then res_point a b
        else (res_intervals a b);;

let ans a b : bool = if hub a b then true else false;;

ans (make_interval (make_point 1 1) (make_point 2 5)) (make_interval (make_point 2 2) (make_point 10 10));;