(* 
Bartosz Kucypera
code reviewe: Jakub Kłos
*)

exception Cykliczne

open Array
open Queue
open PMap
open Printf

let topol (v: ('a * 'a list) list) = 
    (*licznik różnych wierzchołków*)
    let ile = ref 0 in 
    (*lista wynikowa *)
    let res = ref [] in 
    (* trzyma stopnie wejściowe wierzchołków*)
    let sto = ref empty in 
    (*set na krawędzie *)
    let se = ref empty in 
    (*listy sąsiedztwa na mapie*)
    let g = ref empty in 
    (*szuakmy samotnych wierzchołków*)
    let rec lap le = match le with
        | [] -> ()
        | (a, [])::tt -> (if mem a !sto = false then (sto:= (add a 0 !sto);ile:=!ile+1); lap tt)
        | (a, h::t)::tt -> lap tt 
        in lap v;
    (*budujemy seta krawędzi*)
    let rec f e li = 
        match li with
        | [] -> ()
        | h::t ->((se:= (add (e, h) 1 !se));(f e t)) in 
    let rec pom l = match l with 
        | [] -> ()
        | hd::tl -> (f (fst hd) (snd hd); (pom tl))
    in (pom v); 
    (*budujemy mape stopni wierzchołków*)
    let f1 (a, b) _ = 
        begin 
        if(mem a !sto) = false then (sto:= (add a 0 !sto); ile:=!ile+1);
        if (mem b !sto) = false then (sto:= (add b 1 !sto); ile:= !ile+1)
        else (sto:= (add b ((find b !sto)+1) !sto))
        end 
        in (iter f1 !se);
    (*budujemy liste sąsiedztwa na mapie *)
    let f2 (v1, v2) _ = 
        begin 
        (if (mem v1 !g) = false then (g:= (add v1 [] !g)));
        g:= (add v1 (v2::(find v1 !g)) !g);
        end 
        in iter f2 !se;
    (*wrzucamy na kolejke wierzchołki o stopniu 0*)
    let q = Queue.create () in
    let f3 el st = (if st = 0 then (Queue.push el q)) 
        in iter f3 !sto;
    (*puszczamy toposorta bfs-owego*) 
    while (Queue.is_empty q) = false do
        let tp = Queue.take q in 
            res:= (tp::!res);
            (*zmniejszamy stopnie sąsiadom, wrzucamy ich na kolejke jeśli osiągną stopień 0*)
            let f4 ver = 
                begin
                sto:= (add ver ((find ver !sto)-1) !sto);
                if (find ver !sto) = 0 then (Queue.push ver q)
                end in 
            (if(mem tp !g) = true then List.iter f4 (find tp !g))  
    done;
    (*jeśli wynik ma mnej wierzchołków niż mieliśmy na wejściu to jest cykl*)
    if List.length !res <> !ile then raise Cykliczne
    (*jesli nie ma cyklu zwracamy wynik*)
    else List.rev !res;;
(*przechodzi wszystkie testy z gitlaba tylko mieli troche bo stała duża z 3 pMapów*)