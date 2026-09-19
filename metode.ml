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


let izraz_v_vektorje_in_nazaj (i : izraz) : izraz =
    (* za pogrupiranje člene z istim vektorjem: seštevanje njihovih koeficientov *)
    let spr = spremenljivke i
    in
    let seznam_vektorjev = pretvori_v_vektorje spr i
    in
    polinom_v_izraz spr (List.map (fun (prva, druga) -> (prva, druga)) seznam_vektorjev)

let rec main_funkcija (i : izraz) : (racionalno * izraz * izraz) option =
    let spr = spremenljivke i
    in
    let seznam_vektorjev = pretvori_v_vektorje spr i
    in
    let (a, b) = skupni_veckratnik seznam_vektorjev
    in
    let rezultat = najdi b
    in
    match rezultat with
    | Some (x, y) -> Some (
        a,
        polinom_v_izraz spr (List.map (fun (prva, druga) -> (prva, (int_v_rat druga))) x),
        polinom_v_izraz spr (List.map (fun (prva, druga) -> (prva, (int_v_rat druga))) y)
    )
    | None -> None


let poenostavitev (i : izraz) : izraz =  (* P *)
    i |> uporabi_pow |> distribute_polno |> asociiraj_polno |> 
    komutiraj_koeficiente_polno |> poenostavi_izraz_polno |> (* poračunaj koeficiente *)
    spravi_div_zunaj_polno |> (* spravi vse pod en ulomek *)
    komutiraj_koeficiente_polno |> poenostavi_izraz_polno  (* in poskusi še enkrat poračunati koeficiente *)
    |> izraz_v_vektorje_in_nazaj  (* pogrupiraj člene z istimi koeficienti - ampak ker to zna dodati razna množenja z 1 ali seštevanja z 0, ponovimo vse prej še enkrat *)
    |> komutiraj_koeficiente_polno |> poenostavi_izraz_polno |>
    spravi_div_zunaj_polno |>
    komutiraj_koeficiente_polno |> poenostavi_izraz_polno

let rec kompleksnost (i : izraz) : int =
    match i with
    | Rat x -> 1
    | Neznanka x -> 1
    | Plus (a, b) -> kompleksnost a + kompleksnost b
    | Minus (a, b) -> kompleksnost a + kompleksnost b
    | Times (a, b) -> kompleksnost a + kompleksnost b
    | Div (a, b) -> kompleksnost a + kompleksnost b
    | Pow (_, b) -> kompleksnost b
    | Root (_, b) -> kompleksnost b


let pomozna (trojica : racionalno * izraz * izraz) : izraz =
    (* vrne rat * izraz * izraz, ustrezno poenostavljen *)
    let (k, x, y) = trojica
    in
    let x1 = poenostavitev x
    in
    let x2 = poenostavitev (Times (Rat (int_v_rat (-1)), x))
    in
    let y1 = poenostavitev y
    in
    let y2 = poenostavitev (Times (Rat (int_v_rat (-1)), y))
    in
    let c1 = kompleksnost x1 + kompleksnost y1
    in
    let c2 = kompleksnost x2 + kompleksnost y2
    in
    let prvi_faktor = if c1 < c2 then x1 else x2
    in
    let drugi_faktor = if c1 < c2 then y1 else y2
    in
    if k =%= int_v_rat 0 then
        Rat (int_v_rat 0)
    else
        if k =%= int_v_rat 1 then
            Times (
                prvi_faktor,
                drugi_faktor
            )
        else
            if k =%= int_v_rat (-1) then
                Times (
                    poenostavitev (Times (Rat (int_v_rat (-1)), prvi_faktor)),
                    drugi_faktor
                )
            else
                if k <%< int_v_rat 0 then
                    Times (
                        Rat (k *%* (int_v_rat (-1))),
                        Times (
                            poenostavitev (Times (Rat (int_v_rat (-1)), prvi_faktor)),
                            drugi_faktor
                        )
                    )
                else
                    Times (
                        Rat k,
                        Times (
                            prvi_faktor,
                            drugi_faktor
                        )
                    )

let faktorizacija (i : izraz) : izraz * bool =  (* F *)
    (* bool pove, ali je faktorizacija uspela *)
    match (i |> spravi_div_zunaj_polno |> poenostavi_izraz_polno) with
    | Div (a, b) -> (
        match (main_funkcija a, main_funkcija b) with
        | Some t, Some t' -> Div (pomozna t, pomozna t'), true
        | Some t, None -> Div (pomozna t, b), true
        | None, Some t -> Div (a, pomozna t), true
        | None, None -> Div (a, b), false
    )
    | j -> (
        match main_funkcija j with
        | Some t -> pomozna t, true
        | None -> j, false
    )