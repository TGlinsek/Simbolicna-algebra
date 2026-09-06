open Racionalno
open Izraz
open Solver
open Parser


let rec concat_brez_ponavljanj (xs) (ys) =
    (* predpostavljamo, da sta brez ponavljanj *)
    match ys with
    | [] -> xs
    | y :: ys' -> (
        if List.exists (fun x -> y = x) xs then
            concat_brez_ponavljanj xs ys'
        else
            y :: (concat_brez_ponavljanj (xs) ys')
    )

let spremenljivke (izraz : izraz) : string list =
    let rec aux i =
        match i with
        | Neznanka x -> [x] 
        | Rat _ -> []
        | Plus (a, b) -> concat_brez_ponavljanj (aux a) (aux b)
        | Minus (a, b) -> concat_brez_ponavljanj (aux a) (aux b)
        | Times (a, b) -> concat_brez_ponavljanj (aux a) (aux b)
        | Div (a, b) -> concat_brez_ponavljanj (aux a) (aux b)
        | Root (n, i) -> aux i
        | Pow (n, i) -> aux i
    in
    List.sort compare (aux izraz)

let rec je_polinom (i : izraz) : bool =
    match i with
    | Neznanka _ -> true
    | Rat _ -> true
    | Plus (a, b) -> je_polinom a && je_polinom b
    | Minus (a, b) -> je_polinom a && je_polinom b
    | Times (a, b) -> je_polinom a && je_polinom b
    | _ -> false


let rec zamenjaj_pri_indeksu (sez : 'a list) i (f : 'a -> 'a) =
    match i, sez with
    | 0, s :: rest ->
        (f s) :: rest
    | n, x :: rest ->
        x :: zamenjaj_pri_indeksu rest (n - 1) f
    | _, [] ->
        failwith "prevelik indeks"

let rec zdruzi_seznama (a : ('a * 'b) list) (b : ('a * 'b) list) (enakost : 'a -> 'a -> bool) (g : 'b -> 'b -> 'b) =
    match b with
    | [] -> a
    | (x, y) :: xs ->
        match List.find_opt (fun (x', _) -> enakost x x') a with
        | Some (_, _) -> let a' = List.map (
            fun (x'', y'') ->
                if enakost x x'' then
                   (x'', g y y'')
                else
                   (x'', y'')
            ) a
            in
            zdruzi_seznama a' xs enakost g
        | None -> zdruzi_seznama ((x, y) :: a) xs enakost g

let rec seznama_sta_enaka (s1) (s2) : bool =
    match s1, s2 with
    | (x :: xs), (y :: ys) -> x = y && seznama_sta_enaka xs ys
    | [], [] -> true
    | _ -> false

let rec find_index (f : 'a -> bool) (sez : 'a list) : int =
    match sez with
    | x :: xs -> if f x then 0 else (find_index f xs) + 1
    | [] -> failwith "Ni takega indeksa"

let pretvori_v_vektorje spr (i : izraz) : ((int list) * racionalno) list =
    let i' = distribute_polno i
    in
    if not (je_polinom i) then
        failwith "to ni polinom!"
    else
    let rec aux (clen : izraz) v_in_koef =
        (* v_in_koef je par vektorja in koeficienta *)
        match clen with
        | Neznanka x -> (
            let indeks = find_index (fun y -> x = y) spr 
            in
            (zamenjaj_pri_indeksu (fst v_in_koef) indeks (fun z -> z + 1), snd v_in_koef)
        )
        | Rat r -> (fst v_in_koef, (snd v_in_koef) *%* r)
        | Times (a, b) -> (
            aux b (aux a v_in_koef)
        )
        | _ -> failwith ("napaka " ^ (izraz_to_string clen) ^ " !")
    in
    let rec aux' (i : izraz) : ((int list) * racionalno) list =
        match i with
        | Times _ -> [aux i (List.init (List.length spr) (fun x -> 0), int_v_rat 1)]
        | Plus (a, b) -> zdruzi_seznama (aux' a) (aux' b) (seznama_sta_enaka) (fun x y -> x +%+ y)
        | Minus (a, b) -> aux' (Plus (a, (Times (rat (-1) 1, b))))
        | _ -> [aux i (List.init (List.length spr) (fun x -> 0), int_v_rat 1)]
    in
    aux' i'

let skupni_veckratnik (a : ((int list) * racionalno) list) : racionalno * (((int list) * int) list) =
    let sez = List.fold_left (fun acc (prva, druga) -> druga :: acc) [] a
    in
    let r = gcd_rat_seznam sez
    in
    (r, List.map (fun (prva, druga) -> (prva, rat_v_int (druga /%/ r))) a)


let rec na_potenco (x : izraz) n =
    match n with
    | 0 -> rat 1 1
    | _ -> x ** na_potenco x (n - 1)

let rec monom_v_izraz spr (l : int list) =
    match spr, l with
    | [], [] -> rat 1 1
    | spr' :: sprs, l' :: ls -> (na_potenco (Neznanka spr') l') ** (monom_v_izraz sprs ls)
    | _ -> failwith "Neustrezno število spremenljivk"

let rec polinom_v_izraz (spr) (p : (int list * racionalno) list) : izraz =
    match p with
    | (l, r) :: xs -> Plus (Times (Rat r, monom_v_izraz spr l), polinom_v_izraz spr xs)
    | [] -> rat 0 1

let main_funkcija (i : izraz) : (izraz * izraz) option =
    let spr = spremenljivke i
    in
    let (a, b) = skupni_veckratnik (pretvori_v_vektorje spr i)
    in
    let rezultat = najdi b
    in
    match rezultat with
    | Some (x, y) -> Some (
        polinom_v_izraz spr (List.map (fun (prva, druga) -> (prva, (int_v_rat druga) *%* a)) x),
        polinom_v_izraz spr (List.map (fun (prva, druga) -> (prva, (int_v_rat druga) *%* a)) y)
    )
    | None -> None
