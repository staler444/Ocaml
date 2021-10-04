type mx ={
    a11 : int;
    a12 : int;
    a21 : int;
    a22 : int;
};;

let make_mx a11 a12 a21 a22 = {a11 = a11; a12 = a12; a21 = a21; a22 = a22;};;

let mno_mx a b = {
a11 = a.a11*b.a11+a.a21*b.a12; 
a12 = a.a12*b.a11+a.a22*b.a12; 
a21 = a.a21*b.a11+a.a22*b.a21; 
a22 = a.a21*b.a12+a.a22*b.a22;};;

let make_mx_fib = (make_mx 1 1 1 0);;

let rec max_pot n pot = if pot > n then pot else max_pot n pot*2;;

let int_to_bool a b : bool = if a land b == 0 then false else true;;

let rec go (i: int) res pom (n:int) = 
if i > n then res.a22
else if (int_to_bool i n) then go (i*2: int) (mno_mx res pom) (mno_mx pom pom) n
else go (i*2:int) res (mno_mx pom pom) n;;

let rec jazda = go 1 make_mx_fib make_mx_fib;;