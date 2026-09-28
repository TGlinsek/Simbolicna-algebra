
let flatten_map (f : 'a -> 'b list) (l : 'a list) : 'b list =
    (* najprej map, nato flatten *)
    List.fold_left ( @ ) [] (List.map f l)

let find_map (f : 'a -> 'b option) (l : 'a list) : 'b option =
    (* vrne prvi element, ki ga f ne slika v None *)
    let rec aux s =
        match s with
        | Some x :: _ -> Some x
        | None :: s' -> aux s'
        | [] -> None
    in
    aux (List.map f l)


type vektor = int list  (* seznam eksponentov nedoločenk v členu *)
type vektorji = vektor list
type clen = vektor * int  (* int tu predstavlja koeficient pred členom *)
type polinom = clen list
type faktorizacija = polinom * polinom



let rec zmnozi_vektorja (a : vektor) (b : vektor) : vektor =
    (* za množenje členov *)
    match a, b with
    | [], [] -> []
    | x :: xs, y :: ys ->
        (x + y) :: zmnozi_vektorja xs ys
    | _ -> failwith "Vektorja morata biti iste dolžine"


let rec deli_vektorja (a : vektor) (b : vektor) : vektor =
    (* deljenje členov *)
    match a, b with
    | [], [] -> []
    | x :: xs, y :: ys ->
        (x - y) :: deli_vektorja xs ys
    | _ -> failwith "Vektorja morata biti iste dolžine"


let nenegativen (v : vektor) : bool =
    (* vse potence so nenegativne *)
    List.for_all (fun x -> x >= 0) v


let manjsi_ali_enak (a : vektor) (b : vektor) : bool =
    (* potence vektorja a so kvečjemu potence vektorja b *)
    List.for_all2 (fun x y -> x <= y) a b



let enaka (a : vektor) (b : vektor) : bool =
    (* enakost vektorjev *)
    List.for_all2 ( = ) a b



let rec vektor_je_v_seznamu (v : vektor) (sez : vektorji) : bool =
    match sez with
    | [] -> false
    | y :: ys -> enaka v y || vektor_je_v_seznamu v ys


let dodaj_vektor (x : vektor) (s : vektorji) : vektorji =
    (* seznam nima podvojenih vektorjev *)
    if vektor_je_v_seznamu x s then s
    else x :: s


let produkt_vsote_vektorjev (a : vektorji) (b : vektorji) : vektorji =
    (* seznam vseh seštevkov vektorjev iz a in b, tj. produkt monomov *)
    (* produkt a in b: če na a gledamo kot (a_1 + a_2 + ...), na b pa (b_1 + b_2 + ...), dobimo a_1b_1 + a_1b_2 + ... + a_2b_1 + ... *)
    List.fold_left
        (fun s x ->
            List.fold_left
                (fun s' y ->
                    dodaj_vektor (zmnozi_vektorja x y) s'
                ) s b
        ) [] a


let rec pristej_vektor (v : vektor) (p : polinom) : polinom =
    (* v je vektor, tj. monom, ki mu dodelimo koeficient 1 *)
    (* sprejme polinom p, vrne polinom p + 1 * v *)
    match p with
    | [] -> [(v, 1)]
    | (y, n) :: rest ->
        if enaka v y then
            (y, n + 1) :: rest
        else
            (y, n) :: pristej_vektor v rest


(* podobno kot produkt_vsote_vektorjev, le da pri produkt_vsote_vektorjev samo zmnožimo konvolucijsko in se ne oziramo na koeficiente: če obstaja tak člen, potem je koeficient 1 *)
(* tukaj pa upoštevamo koeficiente, zato vrnemo polinom *)
let produkt_vsote_monomov (a : vektorji) (b : vektorji) : polinom =
    (* a in b sta seznama vektorjev, torej ju lahko gledamo kot polinoma, kjer ima vsak unikaten monom koeficient 1 *)
    (* funkcija vrne polinom, ki je produkt teh dveh polinomov *)
    let nicelni_polinom = []
    in
    List.fold_left
        (fun stevci x ->
            List.fold_left (
                fun stevci' y ->
                    pristej_vektor (zmnozi_vektorja x y) stevci'
            ) stevci b
        ) nicelni_polinom a


let monomi (p : polinom) : vektorji =
    (* seznam vseh različnih monomov *)
    List.fold_left (fun s (e, _) -> dodaj_vektor e s) [] p


let se_lahko_odsteje (monomi : vektorji) (a : vektorji) (b : vektorji) : bool =
    (* seznam "monomi" vsebuje vse tiste člene v a*b, ki se pojavijo le enkrat (imajo koeficient 1)*)
    (* npr. za (1 + y)*(x + xy) mora "monomi" vsebovati vsaj vektorja x in xy^2 *)
    let vsote = produkt_vsote_monomov a b
    in
    List.for_all
        (fun (x, n) ->
            vektor_je_v_seznamu x monomi || n >= 2  (* n je vedno >= 1 *)
        ) vsote


let maksimalni (monomi : vektorji) : vektorji =
    (* maksimalni monomi *)
    (* vrne tiste člene x iz seznama "monomi", za katere velja, da v tem seznamu ne obstaja člen, od katerega bi x bil manjši (tj. manjši ali enak v vseh komponentah)*)
    (* členi v "monomi", ki imajo duplikate, vseeno štejemo - sicer pa bo "monomi" praviloma imel same unikatne člene, brez dvojnikov*)
    (* [x, xy, xx] bo imel maksimalne monome xy in xx *)

    List.filter
        (fun x ->
            not (List.exists
                (fun y ->
                    not (enaka x y) &&
                    manjsi_ali_enak x y
                ) monomi
            )
        ) monomi


let rec dekompozicije (m : vektor) : (vektor * vektor) list =
    (* vsi načini, da monom m zapišeš kot produkt dveh monomov a in b (oba z nenegativnimi eksponenti) *)
    match m with
    | [] -> [([], [])]
    | x :: xs ->
        flatten_map
            (fun n -> List.map
                (fun (as_, bs_) ->
                    (n :: as_, (x - n) :: bs_)
                ) (dekompozicije xs)  (* vsaki dekompoziciji za ostale spremenljivke dodamo dekompozicijo, kjer dodamo prvemu x^a, drugemu pa x^(n - a) *)
            )
            (List.init (x + 1) (fun i -> i))  (* to je seznam [0; 1; ... ; x] *)



let rec najdi_nove_monome (monomi : vektorji) (aji : vektorji) (bji : vektorji) : (vektorji * vektorji) list =
    (* aji so za A, bji za B, iščemo f = A*B*)
    let vsote = produkt_vsote_vektorjev aji bji
    in
    match
        List.find_opt
            (fun x -> not (vektor_je_v_seznamu x vsote))
            monomi
    with
    | None ->  (* None pomeni, da vsak element "monomi" najdemo tudi v "vsote" *)
        if se_lahko_odsteje monomi aji bji then
            [(aji, bji)]  (* aji * bji - monomi izniči vse člene v aji*bji, ki so imeli koef. natanko 1. tj. vse tiste je tudi "monomi" imel. Hkrati je vsak člen v "monomi" že bil nekje v "vsote", zato ni kreiral novih členov, le morda je modificiral koefe že obstoječih členov. *)
            (* torej sta aji in bji dobra kandidata za faktorja za "monomi" *)
        else
            []

    | Some m ->  (* m je vektor iz monomov, ki se ne pojavi v "vsote" *)

        (* from_b je seznam parov (vektorji, vektorji) *)

        (* če je bji = [b_1, b_2, ...], potem je from_b = [(m/b_1 :: aji, bji); (m/b_2 :: aji, bji); ...] z vsemi elementi m/b_i, ki še niso v aji, in ki imajo vse potence nenegativne *)
        let from_b = List.filter_map (
                fun y ->
                    let x = deli_vektorja m y
                    in
                    if nenegativen x && not (vektor_je_v_seznamu x aji) then
                        Some (x :: aji, bji)
                    else
                        None
                ) bji  (* iz monoma v Bju poskusi dobiti monom v A*)
        in
        (* še drugemu faktorju dodamo vse možne kvociente *)
        (* m je potem generiran kot y * nek element ajev *)
        let from_a = List.filter_map (
                fun x ->
                    let y = deli_vektorja m x
                    in
                    if nenegativen y && not (vektor_je_v_seznamu y bji) then
                        Some (aji, y :: bji)
                    else
                        None
                ) aji
        in
        (* ker moramo poskrbeti še za m, bomo dodali vse m-jeve dekompozicije zraven. *)
        (* m je potem generiran kot x * y *)
        let nove_dekompozicije =
            List.filter_map
                (fun (x, y) ->
                    if vektor_je_v_seznamu x aji || vektor_je_v_seznamu y bji then
                        None
                    else
                        Some (x :: aji, y :: bji)
                ) (dekompozicije m)
        in
        flatten_map (
            fun (a', b') ->
                najdi_nove_monome monomi a' b'  (* poskusi z a' in b', ki imata več monomov kot a in b *)
            ) (from_b @ from_a @ nove_dekompozicije)


let kandidati_za_nove_monome (monomi : vektorji) : (vektorji * vektorji) list =
    (* za vse različne pare faktorjev (a, b), da je a*b nek maksimalen monom, poženemo najdi_nove_monome *)
    flatten_map (
        fun m -> flatten_map (
                fun (a, b) -> najdi_nove_monome monomi [a] [b]
            ) (dekompozicije m)
    ) (maksimalni monomi)  (* ne rabimo vseh monomov, dovolj je gledat maksimalne *)


let rec isci_koeficiente (monomi : vektorji) (dovoljeni_koeficienti : int list) : polinom list =
    (* iz seznama monomov [m_1, m_2, ..., m_n] naredi seznam polinomov oblike c_1 * m_1 + c_2 * m_2 + ... + c_n * m_n, kjer je (c_1, ..., c_n) nek nabor dovoljenih koeficientov *)
    match monomi with
    | [] -> [[]]
    | eksponent :: es ->
        flatten_map (
            fun koef ->
                List.map (
                    fun p -> (eksponent, koef) :: p
                ) (isci_koeficiente es dovoljeni_koeficienti)
            ) dovoljeni_koeficienti
        (* če imamo seznam polinomov, ki ga dobimo iz es, kakšen je seznam polinomov, če imamo eksponent :: es? (eksponent je tukaj tipa vektor) *)
        (* dobimo seznam polinomov: [p + c*eksponent | p iz (isci_koeficiente es), c koeficient iz dovoljenih koeficientov] *)


let nekonstanten (faktor : polinom) : bool =
    (* polinom ima vsaj en člen, ki vsebuje spremenljivko *)
    List.exists (
        fun (eksponenti, _) ->
            List.exists (fun x -> x <> 0) eksponenti
        ) faktor

let najdi_faktorje preveri (p : polinom) (dovoljeni_koeficienti : int list) : faktorizacija option =
    let s = monomi p
    in
    let rec preizkusi_monom (sez : (vektorji * vektorji) list) =
        match sez with
        | [] -> None
        | (a, b) :: rest ->
            (* seznam polinomov kandidatov, ki so morda faktorji *)
            let kandidat_a =
                isci_koeficiente a dovoljeni_koeficienti  (* seznamu vektorjev a dodelimo polinome tako, da poskusimo vse kombinacije dovoljenih koeficientov *)
            in
            (* seznam polinomov kandidatov, ki so morda faktorji *)
            let kandidat_b =
                isci_koeficiente b dovoljeni_koeficienti
            in
            (* vrne prvi par (faktor_a, faktor_b), ki se dejansko zmnožita v polinom p *)
            let faktorji = (
                find_map (fun faktor_a ->
                    find_map (fun faktor_b ->
                        if nekonstanten faktor_a && nekonstanten faktor_b then
                            (if preveri faktor_a faktor_b p then
                                Some (faktor_a, faktor_b)
                            else
                                None
                            )
                        else None
                    ) kandidat_b
                ) kandidat_a  (* seznam morebitnih faktorjev za a *)
            )
            in
            match faktorji with
            | Some faktorji -> Some faktorji
            | None -> preizkusi_monom rest  (* par seznamov vektorjev (a, b) ni bil ustrezen. glejmo ostale pare *)
    in
    let kandidati = kandidati_za_nove_monome s  (* tukaj ne gledamo koeficientov: samo vektorje *)
    in
    preizkusi_monom kandidati  (* vrne prvi element iz kandidatov, ki prestane vse teste *)


(* preverjanje enakosti zmnožka faktorjev in polinoma *)

let direktno_mnozenje (a : polinom) (b : polinom) : polinom =
    (* vrne zmnožek polinomov a in b *)
    List.fold_left
        (fun acc (ea, ca) ->
            List.fold_left (
                fun acc' (eb, cb) ->
                    let e = zmnozi_vektorja ea eb  (* zmnožimo člena *)
                    in
                    let c = ca * cb  (* zmnožima koeficienta *)
                    in
                    match List.find_opt (fun (e', _) -> enaka e e') acc'  (* najdi e v acc' *)
                    with
                    | Some (e', c') ->
                        (e', c' + c) :: List.remove_assoc e' acc'  (* odstranimo e', da nimamo podvojitev *)
                    | None ->  (* e-ja še ni v acc', zato ga dodamo *)
                        (e, c) :: acc'
                )
            acc
            b
        )
    []
    a


let odstrani_nicle (polinom : polinom) : polinom =
    (* vrne le člene z neničelnim koeficientom *)
    List.filter (fun (_, c) -> c <> 0) polinom


let preveri (a : polinom) (b : polinom) (p : polinom) : bool =
    (* preverimo, ali velja a * b = p *)
    let zmnozek = direktno_mnozenje a b |> odstrani_nicle
    in
    let polinom = odstrani_nicle p
    in
    List.length zmnozek = List.length polinom
    &&
    (* seznama zmnozek in polinom sta permutaciji drug drugega *)
    List.for_all (
            (* vrne true, če v polinomu obstaja element (e, c) *)
            fun (e, c) ->
            match List.find_opt (fun (e', _) -> enaka e e') polinom with
            | Some (_, c') -> c = c'
            | None -> false
        ) zmnozek

(* dovoljeni_koeficienti povejo, katere vse možne koeficiente gledamo pri morebitni faktorizaciji *)
let najdi (p : polinom) (dovoljeni_koeficienti : int list) : faktorizacija option
    = najdi_faktorje preveri p dovoljeni_koeficienti


let rec generiraj_dovoljene_koeficiente (n : int) : int list =
    (* generira množico celih števil v intervalu [-n, n] *)
    match n with
    | 0 -> [0]
    | k -> (-n) :: n :: generiraj_dovoljene_koeficiente (k - 1)
    