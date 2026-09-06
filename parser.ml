open Izraz
open Racionalno

type expression =
    | IntE of int
    | FloatE of float
    | VarE of string
    | PlusE of expression * expression
    | MinusE of expression * expression
    | TimesE of expression * expression
    | DivE of expression * expression
    | PowE of expression * expression
    | RootE of int * expression
    | NegE of expression


type vmesni_izraz =
    | Rat' of racionalno
    | Neznanka' of string
    | Plus' of vmesni_izraz * vmesni_izraz
    | Minus' of vmesni_izraz * vmesni_izraz
    | Times' of vmesni_izraz * vmesni_izraz
    | Div' of vmesni_izraz * vmesni_izraz
    | Root' of int * vmesni_izraz
    | Pow' of vmesni_izraz * vmesni_izraz

let rec prevedi_v_vmesni_izraz (i : expression) : vmesni_izraz =
    match i with
        | IntE n -> (Rat' (int_v_rat n))
        | FloatE f -> Rat' (float_v_rac f)
        | VarE s -> Neznanka' s
        | PlusE (e1, e2) -> Plus' (prevedi_v_vmesni_izraz e1, prevedi_v_vmesni_izraz e2)
        | MinusE (e1, e2) -> Minus' (prevedi_v_vmesni_izraz e1, prevedi_v_vmesni_izraz e2)
        | TimesE (e1, e2) -> Times' (prevedi_v_vmesni_izraz e1, prevedi_v_vmesni_izraz e2)
        | DivE (e1, e2) -> Div' (prevedi_v_vmesni_izraz e1, prevedi_v_vmesni_izraz e2)
        | PowE (e, n) -> Pow' (prevedi_v_vmesni_izraz n, prevedi_v_vmesni_izraz e)
        | RootE (n, e) -> (
            let rez = prevedi_v_vmesni_izraz e
            in
            match rez with
            | x -> Root' (n, x)
        )
        | NegE e -> Minus' ((Rat' (int_v_rat 0)), prevedi_v_vmesni_izraz e)

let rec prevedi_vmesni_izraz_v_izraz (i : vmesni_izraz) : izraz =
    match i with
    | Rat' r -> Rat r
    | Neznanka' s -> Neznanka s
    | Plus' (a, b) -> Plus (prevedi_vmesni_izraz_v_izraz a, prevedi_vmesni_izraz_v_izraz b)
    | Minus' (a, b) -> Minus (prevedi_vmesni_izraz_v_izraz a, prevedi_vmesni_izraz_v_izraz b)
    | Times' (a, b) -> Times (prevedi_vmesni_izraz_v_izraz a, prevedi_vmesni_izraz_v_izraz b)
    | Div' (a, b) -> Div (prevedi_vmesni_izraz_v_izraz a, prevedi_vmesni_izraz_v_izraz b)
    | Root' (n, r) -> Root (n, prevedi_vmesni_izraz_v_izraz r)
    | Pow' (e, b) -> (
        let r = poracunaj_stevilski_izraz (prevedi_vmesni_izraz_v_izraz e)
        in
        Pow (r.stevec, Root (r.imenovalec, prevedi_vmesni_izraz_v_izraz b))  (*Pow (poracunaj_stevilski_izraz e, b)*)
    )
    
type token =
    | FLOAT of string
    | SPREMENLJIVKA of string
    | PLUS | MINUS | TIMES | DIV
    | NA_POTENCO
    | UKLEPAJ | ZAKLEPAJ
    | KONEC

let tokenize (s : string) : token list =
    let len = String.length s 
    in
    let is_digit c = '0' <= c && c <= '9' 
    in
    let is_letter c =('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')
    in
    let rec aux i acc =
        if i >= len then 
            List.rev (KONEC :: acc)  (* akumulator je seznam vseh tokenov, samo obrniti ga je treba *)
        else
            match s.[i] with
                | ' ' -> aux (i + 1) acc
                | '\t' -> aux (i + 1) acc
                | '\n' -> aux (i + 1) acc
                | '+' -> aux (i + 1) (PLUS :: acc)
                | '-' -> aux (i + 1) (MINUS :: acc)
                | '*' -> aux (i + 1) (TIMES :: acc)
                | '/' -> aux (i + 1) (DIV :: acc)
                | '(' -> aux (i + 1) (UKLEPAJ :: acc)
                | ')' -> aux (i + 1) (ZAKLEPAJ :: acc)
                | '^' -> aux (i + 1) (NA_POTENCO :: acc)
                | c when is_digit c ->
                    let j = ref i 
                    in
                    while !j < len &&(is_digit s.[!j] || s.[!j] = '.') do
                        j := !j + 1
                    done;
                    let f = (String.sub s i (!j - i)) 
                    in
                    aux !j (FLOAT f :: acc)
                | c when is_letter c ->
                    let j = ref i in
                    while !j < len && is_letter s.[!j] do  (* pridobi dolžino imena spremenljivke *)
                        j := !j + 1
                    done;
                    let id = String.sub s i (!j - i) 
                    in
                    let tok =
                        SPREMENLJIVKA id
                    in
                    aux !j (tok :: acc)
                | c ->
                    failwith ("Nepričakovan znak: " ^ String.make 1 c)
    in
    aux 0 []


let vrni_ustrezen_stevilski_tip (s : string) =
    try
        IntE (int_of_string s)
    with _ -> (
        try
            FloatE (float_of_string s)
        with _ -> failwith "Slab format za število"
    )

let rec parsaj_stevilo tokens =
    match tokens with
    | FLOAT f :: rest -> (vrni_ustrezen_stevilski_tip f, rest)
    | SPREMENLJIVKA s :: rest -> failwith "V potenci ne more biti spremenljivk"
    | UKLEPAJ :: rest -> (
        let (e, rest') = parsaj_stevilski_izraz rest
        in
        match rest' with
        | ZAKLEPAJ :: rest'' -> (e, rest'')
        | _ -> failwith "Napaka, kje je zaklepaj?"
    )
    | _ -> failwith "Napaka"
and parsaj_minus_stevila tokens =
    match tokens with
    | MINUS :: rest ->
        let (e, rest') = parsaj_minus_stevila rest 
        in
        (NegE e, rest')
    | tokens ->
        parsaj_stevilo tokens
and parsaj_stevilski_clen tokens =
    let rec aux left rest =
        match rest with
        | TIMES :: rest' ->
            let (right, rest'') = parsaj_minus_stevila rest' 
            in
            aux (TimesE (left, right)) rest''
        | DIV :: rest' ->
            let (right, rest'') = parsaj_minus_stevila rest' 
            in
            aux (DivE (left, right)) rest''
        | rest -> (left, rest)
    in
    let (left, rest) = parsaj_minus_stevila tokens 
    in
    aux left rest
and parsaj_stevilski_izraz tokens =
    let rec aux left rest =
        match rest with
        | PLUS :: rest' ->
            let (right, rest'') = parsaj_stevilski_clen rest' 
            in
            aux (PlusE (left, right)) rest''
        | MINUS :: rest' ->
            let (right, rest'') = parsaj_stevilski_clen rest' 
            in
            aux (MinusE (left, right)) rest''
        | rest -> (left, rest)
    in
    let (left, rest) = parsaj_stevilski_clen tokens 
    in
    aux left rest

let rec parsaj_literal tokens =
    match tokens with
    | FLOAT f :: rest -> (vrni_ustrezen_stevilski_tip f, rest)
    | SPREMENLJIVKA s :: rest -> (VarE s, rest)
    | UKLEPAJ :: rest -> (
        let (e, rest') = parsaj_izraz rest 
        in
        match rest' with
        | ZAKLEPAJ :: rest'' -> (e, rest'')
        | _ -> failwith "Napaka, kje je zaklepaj?"
    )
    | _ -> failwith "Napaka"
and parsaj_potenco tokens =
    let (base, rest) = parsaj_literal tokens 
    in
    match rest with
    | NA_POTENCO :: FLOAT n :: rest' ->
        (PowE (base, IntE (int_of_string n)), rest')  (* n mora predstavljat int, sicer ne gre *)
    | NA_POTENCO :: UKLEPAJ :: rest' -> (
        let (e, rest'') = parsaj_stevilski_izraz rest'
        in
        match rest'' with
        | ZAKLEPAJ :: rest''' -> (PowE (base, e), rest''')
        | _ -> failwith "Napaka, kje je zaklepaj?"
    )
    | _ -> (base, rest)
and parsaj_minus tokens =
    match tokens with
    | MINUS :: rest ->
        let (e, rest') = parsaj_minus rest 
        in
        (NegE e, rest')
    | tokens ->
        parsaj_potenco tokens
and parsaj_clen tokens =
    let rec aux left rest =
        match rest with
        | TIMES :: rest' ->
            let (right, rest'') = parsaj_minus rest' 
            in
            aux (TimesE (left, right)) rest''
        | DIV :: rest' ->
            let (right, rest'') = parsaj_minus rest' 
            in
            aux (DivE (left, right)) rest''
        | rest -> (left, rest)
    in
    let (left, rest) = parsaj_minus tokens 
    in
    aux left rest
and parsaj_izraz tokens =
    let rec aux left rest =
        match rest with
        | PLUS :: rest' ->
            let (right, rest'') = parsaj_clen rest'
            in
            aux (PlusE (left, right)) rest''
        | MINUS :: rest' ->
            let (right, rest'') = parsaj_clen rest'
            in
            aux (MinusE (left, right)) rest''
        | rest -> (left, rest)
    in
    let (left, rest) = parsaj_clen tokens 
    in
    aux left rest

let parsaj (s : string) : expression =
    let tokens = tokenize s 
    in
    let (izraz, rest) = parsaj_izraz tokens
    in
    match rest with
    | [KONEC] -> izraz
    | _ -> failwith "Slab izraz"

let parsaj_v_izraz (s : string) : izraz =
    s |> parsaj |> prevedi_v_vmesni_izraz |> prevedi_vmesni_izraz_v_izraz