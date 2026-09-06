
let concat_map (f : 'a -> 'b list) (l : 'a list) : 'b list =
    List.fold_left ( @ ) [] (List.map f l)

let find_map (f : 'a -> 'b option) (l : 'a list) : 'b option =
    let rec aux s =
        match s with
        | Some x :: _ -> Some x
        | None :: s' -> aux s'
        | [] -> None
    in
    aux (List.map f l)


type vektor = int list
type clen = vektor * int
type polinom = clen list



let rec sestej_vektorja a b =
    match a, b with
    | [], [] -> []
    | x :: xs, y :: ys ->
        (x + y) :: sestej_vektorja xs ys
    | _ -> failwith "Vektorja morata biti iste dolžine"


let rec odstej_vektorja a b =
    match a, b with
    | [], [] -> []
    | x :: xs, y :: ys ->
        (x - y) :: odstej_vektorja xs ys
    | _ -> failwith "Vektorja morata biti iste dolžine"


let nenegativen v =
    (* vse potence so nenegativne *)
    List.for_all (fun x -> x >= 0) v


let manjsi_ali_enak a b =
    (* potence vektorja a so kvečjemu potence vektorja b *)
    List.for_all2 (fun x y -> x <= y) a b



let enaka a b =
    (* enakost vektorjev *)
    List.for_all2 ( = ) a b



let rec vektor_je_v_seznamu v sez = 
    match sez with
    | [] -> false
    | y :: ys -> enaka v y || vektor_je_v_seznamu v ys


let dodaj_vektor x s =
    if vektor_je_v_seznamu x s then s
    else x :: s



let monomi polinom =
    (* seznam vseh različnih monomov *)
    List.fold_left (fun s (e, _) -> dodaj_vektor e s) [] polinom


let monomi_vsota a b =
    (* seznam vseh seštevkov vektorjev iz a in b *)
    List.fold_left
        (fun s x ->
            List.fold_left
                (fun s y ->
                    dodaj_vektor (sestej_vektorja x y) s)
                s
                b)
        []
        a


let rec pristej_eksponent v sez =
    (* povečaj število eksponentov za 1. Če še ni v seznamu, ga dodaj *)
    match sez with
    | [] -> [(v, 1)]
    | (y, n) :: rest ->
        if enaka v y then
            (y, n + 1) :: rest
        else
            (y, n) :: pristej_eksponent v rest



let prestej_vsote_eksponentov (a : vektor list) (b : vektor list) : polinom =
    List.fold_left
        (fun stevci x ->
            List.fold_left (   
                fun stevci' y ->
                    pristej_eksponent (sestej_vektorja x y) stevci'
            )
            stevci
            b
        ) [] a


let veckratnost x stevec =
    match List.find_opt
        (fun (y, _) -> enaka x y)
        stevec
    with
    | Some (_, n) -> n
    | None -> 0



let se_lahko_odsteje monomi a b =
    let vsote = prestej_vsote_eksponentov a b
    in
    List.for_all
        (fun (x, n) ->
            vektor_je_v_seznamu x monomi || n >= 2)
        vsote


let maksimalni monomi =
    (* maksimalni monomi *)
    List.filter
        (fun x ->
            not
                (List.exists
                    (fun y ->
                        not (enaka x y) &&
                        manjsi_ali_enak x y)
                    monomi)
        ) monomi


let rec dekompozicije m =
    (* vsi načini, da m zapišeš kot vsoto a in b *)
    match m with
    | [] -> [([], [])]
    | x :: xs ->
        concat_map
            (fun a -> List.map
                (fun (as_, bs) ->
                    (a :: as_, (x - a) :: bs)
                ) (dekompozicije xs)
            )
            (List.init (x + 1) (fun y -> y))


let rec najdi_nove_monome monomi aji bji =
    (* aji so za A, bji za B, iščemo f = A*B*)
    let vsote = monomi_vsota aji bji in

    match
        List.find_opt
            (fun x -> not (vektor_je_v_seznamu x vsote))
            monomi
    with

    | None ->
        if se_lahko_odsteje monomi aji bji then
            [(aji, bji)]
        else
            []

    | Some m ->
        let from_b = List.filter_map (
                fun y ->
                    let x = odstej_vektorja m y
                    in
                    if nenegativen x && not (vektor_je_v_seznamu x aji) then
                        let aji' = dodaj_vektor x aji 
                        in
                        Some (aji', bji)
                    else
                        None
                ) bji  (* iz monoma v Bju poskusi dobiti monom v A*)
        in

        let from_a = List.filter_map (
                fun x ->
                    let y = odstej_vektorja m x
                    in
                    if nenegativen y && not (vektor_je_v_seznamu y bji) then
                        let bji' = dodaj_vektor y bji 
                        in
                        Some (aji, bji')
                    else
                        None
                ) aji
        in
        let nove_dekompozicije =
            dekompozicije m |> List.filter_map
                (fun (x, y) ->
                    if vektor_je_v_seznamu x aji || vektor_je_v_seznamu y bji then
                        None
                    else
                        let a' = dodaj_vektor x aji in
                        let b' = dodaj_vektor y bji in
                        Some (a', b')
                )
            in
            concat_map (
                fun (a', b') ->
                    najdi_nove_monome monomi a' b'
                ) (from_b @ from_a @ nove_dekompozicije)


let kandidati_za_nove_monome monomi =
    maksimalni monomi |> concat_map (
        fun m -> dekompozicije m |> concat_map
            (fun (a, b) -> najdi_nove_monome monomi [a] [b])
    )


(* tu nastavimo, katere vse možne koeficiente gledamo *)
let dovoljeni_koeficienti =
    [-5; -4; -3; -2; -1; 1; 2; 3; 4; 5]


let rec isci_koeficiente monomi =
    match monomi with
    | [] -> [[]]
    | eksponent :: es ->
        concat_map (
            fun koef ->
                List.map (
                    fun koefi -> (eksponent, koef) :: koefi
                ) (isci_koeficiente es)
            ) dovoljeni_koeficienti


let nekonstanten faktor =
    List.exists (
        fun (eksponenti, _) ->
            List.exists (fun x -> x <> 0) eksponenti
        ) faktor

let najdi_faktorje preveri polinom =
    let s = monomi polinom in

    let rec preizkusi_monom sez =
        match sez with
        | [] -> None
        | (a, b) :: rest ->
            let kandidat_a =
                isci_koeficiente a
            in
            let kandidat_b =
                isci_koeficiente b
            in
            let faktorji = (
                find_map (fun faktor_a ->
                    find_map (fun faktor_b ->
                        if nekonstanten faktor_a && nekonstanten faktor_b then
                            (if preveri faktor_a faktor_b polinom then
                                Some (faktor_a, faktor_b)
                            else
                                None)
                        else None
                    ) kandidat_b
                ) kandidat_a
            )
            in
            match faktorji with
            | Some faktorji -> Some faktorji
            | None -> preizkusi_monom rest
    in
    preizkusi_monom (kandidati_za_nove_monome s)


(* preverjanje enakosti zmnožka faktorjev in polinoma *)

let direktno_mnozenje a b =
    List.fold_left
        (fun acc (ea, ca) ->
            List.fold_left (
                fun acc' (eb, cb) ->
                    let e = sestej_vektorja ea eb in
                    let c = ca * cb in

                    match List.find_opt (fun (e', _) -> enaka e e') acc'
                    with
                    | Some (e', c') ->
                        (e', c' + c) :: List.remove_assoc e' acc'  (* odstranimo e', da nimamo podvojitev *)
                    | None ->
                        (e, c) :: acc'
                )
            acc
            b
        )
    []
    a


let odstrani_nicle polinom =
    List.filter (fun (_, c) -> c <> 0) polinom


let preveri a b polinom =
    let zmnozek = direktno_mnozenje a b |> odstrani_nicle
    in
    let polinom = odstrani_nicle polinom
    in
    List.length zmnozek = List.length polinom
    &&
    List.for_all
        (
            fun (e, c) ->
            match List.find_opt (fun (e', _) -> enaka e e') polinom with
            | Some (_, c') -> c = c'
            | None -> false
        )
        zmnozek

let najdi p = najdi_faktorje preveri p
