(* Bartosz Kucypera*)

(*maxymalne wartosci kubków*)
let cap = ref [||];;
(*wartosci końcowe kubków*)
let res = ref [||];;
(*kolejka do bfsa*)
let q = Queue.create ();;
(*dpek do bfsa; klucz = stan kubków; wartość = ilość ruchów*)
let (dp : (int array, int) Hashtbl.t) = Hashtbl.create 100000;;
let n = ref 0;;
let l_gcd = ref 1;;

(*napełniamy i-ty kubek*)
let dolewka vd i = (vd.(i)<-((!cap).(i)); vd);;
(*opróżniamy i-ty kubek*) 
let wylewka vd i = (vd.(i)<-0; vd);;
(*przelewamy z j do i*)
let przelewka vd i j = 
    let newi = (min (!cap).(i) (vd.(i)+vd.(j))) in
    let newj = vd.(j)-(newi-vd.(i)) in (vd.(i)<-newi;vd.(j)<-newj; vd);;
(*update stanu dp i kolejki*)
let up (vd: int array) (res:int) =
    if ((Hashtbl.mem dp vd) <> true) then ((Queue.add (vd, res) q); (Hashtbl.add dp vd res));;

let rec gcd a b = 
    if a = 0 then b 
    else gcd (b mod a) a;;

(*faktyczne liczenie wyniku bfesem*)
let jazda () =
    Hashtbl.clear dp;
    Queue.clear q;
    (up (Array.make !n 0) 0);
    (*iterujemy się póki możemy i póki nie znaleźliśmy wyniku*)
    while ((Queue.is_empty q) <> true) && ((Hashtbl.mem dp !res) <> true) do 
        let tp = Queue.pop q in
        for i = 0 to !n-1 do 
            for j = 0 to !n-1 do 
                if i <> j then (up (przelewka (Array.copy (fst tp)) i j) ((snd tp)+1));
            done
        done;
        for i = 0 to !n-1 do 
            begin
            (up (dolewka (Array.copy (fst tp)) i) ((snd tp)+1));
            (up (wylewka (Array.copy (fst tp)) i) ((snd tp)+1));
            end
        done
    done; 
    (*jeśli jest wartość od stanu końcowego w dp to wypisujemy jeśli nie to sie nie da*)
    if Hashtbl.mem dp !res then (Hashtbl.find dp !res)
    else (-1);;    

(*odsianie corner case-ów*)
let przelewanka ta =
    let senko = ref false in
    n:= Array.length ta;
    cap:= Array.make !n 0;
    res:= Array.make !n 0;
    (*przypadek z brakiem kubków*)
    if !n = 0 then 0 
    else 
    begin
    (*Przypadek z gcd: gdy gcd maksymalnych pojemności nie dzieli którejś z końcowych wartości kubków to się nie da
      Przypadek z pustym pełnym/kubkiem: gdy w stanie końcowym nie ma chociaż jednego pustego albo pełnego kubka to też się nie da*)
    for i = 0 to !n-1 do 
        (!cap).(i)<-(fst ta.(i));
        (!res).(i)<-(snd ta.(i));
    done;
    l_gcd:= (!cap).(0);
    for i = 1 to !n-1 do
        l_gcd:= gcd (min !l_gcd (!cap).(i)) (max !l_gcd (!cap).(i)); 
    done;
    for i = 0 to !n-1 do
        if (((snd ta.(i)) = 0) || ((snd ta.(i)) = (fst ta.(i)))) then senko:= true;
    done;
    for i = 0 to !n-1 do
        if((!res).(i) <> 0) && (((!res).(i) mod !l_gcd) <> 0) then senko:= false;
    done;
    if !senko = false then (-1)
    else jazda ()
    end;;