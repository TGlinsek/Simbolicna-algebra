open Racionalno
open Izraz
open Solver
open Parser
open Pomozne




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







let pretvori_v_vektorje spr (i : izraz) : (vektor * racionalno) list =
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
    let rec aux' (i : izraz) : (vektor * racionalno) list =
        match i with
        | Times _ -> [aux i (List.init (List.length spr) (fun x -> 0), int_v_rat 1)]
        | Plus (a, b) -> zdruzi_seznama (aux' a) (aux' b) (seznama_sta_enaka) (fun x y -> x +%+ y)
        | Minus (a, b) -> aux' (Plus (a, (Times (rat (-1) 1, b))))
        | _ -> [aux i (List.init (List.length spr) (fun x -> 0), int_v_rat 1)]
    in
    aux' i'

let skupni_veckratnik (a : (vektor * racionalno) list) : racionalno * ((vektor * int) list) =
    let sez = List.fold_left (fun acc (prva, druga) -> druga :: acc) [] a
    in
    let r = gcd_rat_seznam sez
    in
    (r, List.map (fun (prva, druga) -> (prva, rat_v_int (druga /%/ r))) a)


let rec na_potenco (x : izraz) n =
    match n with
    | 0 -> rat 1 1
    | _ -> x ** na_potenco x (n - 1)

let rec monom_v_izraz spr (l : vektor) =
    match spr, l with
    | [], [] -> rat 1 1
    | spr' :: sprs, l' :: ls -> (na_potenco (Neznanka spr') l') ** (monom_v_izraz sprs ls)
    | _ -> failwith "Neustrezno število spremenljivk"

let rec polinom_v_izraz (spr) (p : (vektor * racionalno) list) : izraz =
    match p with
    | (l, r) :: xs -> Plus (Times (Rat r, monom_v_izraz spr l), polinom_v_izraz spr xs)
    | [] -> rat 0 1


let izraz_v_vektorje_in_nazaj (i : izraz) : izraz =
    (* za pogrupiranje členov z istim vektorjem: seštevanje njihovih koeficientov *)
    try
        let spr = spremenljivke i
        in
        let seznam_vektorjev = pretvori_v_vektorje spr i
        in
        polinom_v_izraz spr (List.map (fun (prva, druga) -> (prva, druga)) seznam_vektorjev)
    with
    | Failure _ -> i  (* če i ni polinom, potem vrže napako, zato vrnemo kar prvotni izraz *)

let rec main_funkcija (i : izraz) (n : int) : (racionalno * izraz * izraz) option =
    (* vrne faktorizacijo, če jo algoritem najde *)

    (* spremenljivke, ki se pojavijo v izrazu*)
    let spr = spremenljivke i
    in
    (* seznam vektorjev členov, ki se pojavijo v izrazu *)
    let seznam_vektorjev = pretvori_v_vektorje spr i
    in
    (* izpostavimo največji skupni delitelj (tako da so vsi koeficienti cela števila) *)
    let (a, b) = skupni_veckratnik seznam_vektorjev
    in
    let dovoljeni_koeficienti = generiraj_dovoljene_koeficiente n
    in
    (* algoritem za faktoriziranje *)
    let rezultat = najdi b dovoljeni_koeficienti
    in
    match rezultat with
    | Some (x, y) ->  (* trojica (koeficient, faktor1, faktor2) *)
        Some (
            a,
            polinom_v_izraz spr (List.map (fun (prva, druga) -> (prva, (int_v_rat druga))) x),
            polinom_v_izraz spr (List.map (fun (prva, druga) -> (prva, (int_v_rat druga))) y)
        )
    | None -> None  (* faktorizacija ni bila najdena *)



let veckratnost (x : vektor) (polinom : (vektor * 'a) list) : 'a =
    match List.find_opt
        (fun (y, _) -> enaka x y)
        polinom
    with
    | Some (_, a) -> a
    | None -> int_v_rat 0


let rec poenostavi_ulomke_v_izrazu (i : izraz) : izraz =
    match i with
    | Div (a, b) -> (
        let a' = poenostavi_ulomke_v_izrazu a in
        let b' = poenostavi_ulomke_v_izrazu b
        in
        let spr = spremenljivke i
        in
        let seznam_vektorjev_stevec = pretvori_v_vektorje spr a'
        in
        let seznam_vektorjev_imenovalec = pretvori_v_vektorje spr b'
        in
        (* seznam razmerij med koeficienti (za isti člen) *)
        let seznam = List.map 
            (fun (v, k) -> (veckratnost v seznam_vektorjev_stevec) |> ( /%/ ) k) 
            seznam_vektorjev_imenovalec
        in
        let razlika = List.filter 
            (fun (v, _) -> (veckratnost v seznam_vektorjev_imenovalec) =%= (int_v_rat 0)) 
            seznam_vektorjev_stevec
        in
        let polinom_razlika = polinom_v_izraz spr (List.map (fun (prva, druga) -> (prva, druga)) razlika)
        in
        match vsi_elementi_so_enaki ( =%= ) seznam with
        | Some r -> Plus (Rat (inverse_rat r), Div (polinom_razlika, b'))
        | None -> i
    )
    | Plus (a, b) -> Plus (poenostavi_ulomke_v_izrazu a, poenostavi_ulomke_v_izrazu b)
    | Minus (a, b) -> Minus (poenostavi_ulomke_v_izrazu a, poenostavi_ulomke_v_izrazu b)
    | Times (a, b) -> Times (poenostavi_ulomke_v_izrazu a, poenostavi_ulomke_v_izrazu b)
    | Pow (n, b) -> Pow (n, poenostavi_ulomke_v_izrazu b)
    | Root (n, b) -> Pow (n, poenostavi_ulomke_v_izrazu b)
    | _ -> i

let poenostavitev (i : izraz) : izraz =  (* P *)
    (* kompliciran način, da poenostaviš izraz *)

    i |> uporabi_pow |> distribute_polno |> asociiraj_polno |> 
    komutiraj_koeficiente_polno |> poenostavi_izraz_polno |>  (* poračunaj koeficiente *)
    poenostavi_ulomke_v_izrazu |> (* odstranil spravi_div_zunaj_polno, ker samo poveča izraz (da vse ulomke na skupni imenovalec). Iz nekega razloga se potem včasih ne poenostavi več nazaj *)
    
    komutiraj_koeficiente_polno |> poenostavi_izraz_polno |>  (* in poskusi še enkrat poračunati koeficiente *)
    izraz_v_vektorje_in_nazaj |>  (* pogrupiraj člene z istimi koeficienti - ampak ker to zna dodati razna množenja z 1 ali seštevanja z 0, ponovimo vse prej še enkrat *)
    
    komutiraj_koeficiente_polno |> poenostavi_izraz_polno |>
    poenostavi_ulomke_v_izrazu |>
    komutiraj_koeficiente_polno |> poenostavi_izraz_polno

let rec kompleksnost (i : izraz) : int =
    (* pomožna funkcija za izbor manj kompleksnega zapisa faktorja *)
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
    (* preverimo, ali je f ali (-f) bolj smiseln zapis za faktor*)
    let x1 = poenostavitev x
    in
    let x2 = poenostavitev (Times (Rat (int_v_rat (-1)), x))
    in
    (* enako za drug faktor *)
    let y1 = poenostavitev y
    in
    let y2 = poenostavitev (Times (Rat (int_v_rat (-1)), y))
    in
    (* kompleksnost zapisa: možnost 1 *)
    let c1 = kompleksnost x1 + kompleksnost y1
    in
    (* možnost 2 *)
    let c2 = kompleksnost x2 + kompleksnost y2
    in
    (* izberemo faktor na podlagi kompleksnosti *)
    let prvi_faktor = if c1 < c2 then x1 else x2
    in
    let drugi_faktor = if c1 < c2 then y1 else y2
    in
    if k =%= int_v_rat 0 then  (* če je koeficient pred faktorjema enak 0 (to se načeloma ne bo dogajalo) *)
        Rat (int_v_rat 0)
    else
        if k =%= int_v_rat 1 then  (* če je koeficient pred faktorjema enak 1, ga samo ne vključimo v zapis *)
            Times (
                prvi_faktor,
                drugi_faktor
            )
        else
            if k =%= int_v_rat (-1) then  (* če je koeficient -1, ga raje kar damo v enega izmed faktorjev *)
                Times (
                    poenostavitev (Times (Rat (int_v_rat (-1)), prvi_faktor)),
                    drugi_faktor
                )
            else
                if k <%< int_v_rat 0 then  (* če je koeficient negativen, minus predstavimo v prvi faktor *)
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

let faktorizacija (i : izraz) (n : int) : izraz * bool =  (* F *)
    (* n je zgornja meja, do koder iščemo koeficiente (za koeficient k velja |k| <= n) *)
    (* bool pove, ali je faktorizacija uspela *)
    match (i |> spravi_div_zunaj_polno |> poenostavi_izraz_polno) with
    | Div (a, b) -> (* če faktoriziramo ulomek, potem faktoriziramo tako števec kot imenovalec, v kolikor to gre. *)
    (
        match (main_funkcija a n, main_funkcija b n) with
        | Some t, Some t' -> Div (pomozna t, pomozna t'), true
        | Some t, None -> Div (pomozna t, b), true
        | None, Some t -> Div (a, pomozna t), true
        | None, None -> Div (a, b), false
    )
    | j ->  (* če ni ulomek, potem faktoriziramo kot običajno *)
    (
        match main_funkcija j n with
        | Some t -> pomozna t, true
        | None -> j, false
    )