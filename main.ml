open Racionalno
open Izraz
open Kompozicije
open Shramba
open Parser


type inp = (* input iz vmesnika *)
    | NovaEnacba of enacba
    | PreoblikujEnacbo of manipuliraj_enacbo * pointer
    | OperacijaNaEnacbah of operacija * pointer * pointer
    | DodajNeznanko of string
    | DodajSpremenljivko of string * izraz  (* vedno bodo spremenljivke imele vrednost - lahko so None, ampak ne če dobimo iz inputa *)

let substring_do_konca (i : string) (n : int) =
    String.sub i n (String.length i - n)

let parse_izraz (s : string) : izraz =
    Parser.parse s |> Parser.prevedi_v_izraz  (* ni spremenljivk, samo neznanke so. To moramo še pohendlat *)

let parse_enacba (s : string) : enacba =
    try
        let i = String.index s '='
        in
        let levi_izraz = String.sub s 0 i |> parse_izraz
        in
        let desni_izraz = substring_do_konca s (i + 1) |> parse_izraz
        in
        (levi_izraz, desni_izraz)
    with _ -> 
        failwith "To ni enačba!"

let string_v_stevilo (s : string) : stevilo =
    try
        (Rat (int_v_rat (int_of_string s)))
    with _ -> (
        try
            Dec (float_of_string s)
        with _ -> failwith "Slab format za število"
    )

let parsaj_input (i : string) : inp =
    (* sprejme enačbo in kateri gumb smo kliknali (pač, vse kar je relevantno za ocaml za vedet) *)
    (* vrne sparsano, informacije, ki so relevantne za python (sj python ve, kaj je dal ocamlu za poračunat) *)

    (* razdeli string na ukaz in argumente: A 12,45 *)
    let inputek : inp = (
        match String.sub i 0 2 with
        | "N " -> (*dodaj_v_okolje (ZacetniRezultat (substring_do_konca i 2))*) NovaEnacba (parse_enacba (substring_do_konca i 2))
        | "X " -> DodajNeznanko (String.sub i 0 2)
        | "Y " -> DodajSpremenljivko ((
            let indeks = String.index i ','
            in
            let (leva, desna) = (String.sub i 2 indeks, Stevilo (substring_do_konca i (indeks + 1) |> string_v_stevilo))
            in
            leva
        ), (
            let indeks = String.index i ','
            in
            let (leva, desna) = (String.sub i 2 indeks, Stevilo (substring_do_konca i (indeks + 1) |> string_v_stevilo))
            in
            desna
        ))
        | "A " -> PreoblikujEnacbo (PreoblikujObeStrani (Preoblikuj (A, Identiteta)), substring_do_konca i 2 |> int_of_string)
        | "B " -> PreoblikujEnacbo (PreoblikujObeStrani (Preoblikuj (B, Identiteta)), substring_do_konca i 2 |> int_of_string)
        | "C " -> PreoblikujEnacbo (PreoblikujObeStrani (Preoblikuj (C, Identiteta)), substring_do_konca i 2 |> int_of_string)
        | "D " -> PreoblikujEnacbo (Obrni, substring_do_konca i 2 |> int_of_string)
        | "E " -> PreoblikujEnacbo (NaLevo, substring_do_konca i 2 |> int_of_string)
        | "+ " | "- " | "* " | "/ " as znak -> (
            let indeks = String.index i ','
            in
            let (leva, desna) = (String.sub i 2 indeks |> int_of_string, substring_do_konca i (indeks + 1) |> int_of_string)
            in
            match znak with
            | "+ " -> OperacijaNaEnacbah (Sestej, leva, desna)
            | "- " -> OperacijaNaEnacbah (Odstej, leva, desna)
            | "* " -> OperacijaNaEnacbah (Zmnozi, leva, desna)
            | "/ " -> OperacijaNaEnacbah (Deli, leva, desna)
            | _ -> failwith "Neznana operacija"
        )
        | _ -> failwith "Neznan ukaz"
    )
    in inputek


let najdi (a : pointer) : rezultat = 
    let rec aux (sez : rezultat list) (i : pointer) : rezultat =
        match (sez, i) with
        | [], _ -> failwith "prazen seznam"
        | x :: _, 0 -> x
        | x :: xs, i when i > 0 -> aux xs (i - 1)
        | _ -> failwith "Napaka, slab indeks"
    in
    aux (!spremenljivo_okolje).shramba a


let apply (i : izraz) (p : preoblikuj) : izraz =
    match p with
    | A -> Izraz.distribute_polno i
    | B -> Izraz.spravi_div_zunaj i
    | C -> Izraz.asociiraj_polno i
    (* | D -> neznanke_na_levo i  (* v posameznem izrazu jih da na levo *)  *)
    

(*
let preob (i : izraz) (p : preoblikuj) : izraz =
    match p with
    | Faktoriziraj -> Neznanka "sss"
    |_->failwith ""

let rec joj (i : izraz) (u : manipuliraj_izraz) : izraz =
    match u with
    | Identiteta -> i
    | Preoblikuj (pr, man) -> preob (joj i man) pr

let evaluacija (e : enacba) (u : manipuliraj_enacbo) : izraz =
    match u with
    | PreoblikujLevoStran x -> joj (fst e) x
    | _-> failwith ""

(evaluacija enacba (PreoblikujLevoStran (Preoblikuj (Faktoriziraj, Identiteta))),
    Neznanka "aaa"
)

*)


let rec uporabi (enacba : enacba) (ukaz : manipuliraj_enacbo) : enacba = 
    match ukaz with
    | Identiteta -> enacba
    | PreoblikujLevoStran manipulacija ->
        let rec aux (man : manipuliraj_izraz) (e : enacba) = (
            match man with
            | Identiteta -> e
            | Preoblikuj (pr, man) -> (let en = aux man e
            in
            (apply (fst en) pr, snd en))
        )
        in aux manipulacija enacba

    | PreoblikujDesnoStran manipulacija ->
        let rec aux (man : manipuliraj_izraz) (e : enacba) = (
            match man with
            | Identiteta -> e
            | Preoblikuj (pr, man) -> (let en = aux man e
            in
            (fst en, apply (snd en) pr))
        )
        in aux manipulacija enacba

    | PreoblikujObeStrani manipulacija ->
        uporabi (uporabi enacba (PreoblikujLevoStran manipulacija)) (PreoblikujDesnoStran manipulacija)
    | Obrni -> (snd enacba, fst enacba)
    | NaLevo -> (Minus (fst enacba, snd enacba), Stevilo (Rat (int_v_rat 0)))
    | _ -> failwith "Ni implementirano"

let uporabi_op (ena1 : enacba) (ena2 : enacba) (ukaz : operacija) : enacba = 
    let (l1, d1) = ena1
    in
    let (l2, d2) = ena2
    in
    match ukaz with
    | Sestej -> (Plus (l1, l2), Plus (d1, d2))
    | Odstej -> (Minus (l1, l2), Minus (d1, d2))
    | Zmnozi -> (Times (l1, l2), Times (d1, d2))
    | Deli -> (Div (l1, l2), Div (d1, d2))

let poisci_neznanke (en : enacba) : string list = najdi_neznanke_v_izrazu (fst en) @ najdi_neznanke_v_izrazu (snd en)
let poisci_spremenljivke (en : enacba) : string list = najdi_spremenljivke_v_izrazu (fst en) @ najdi_spremenljivke_v_izrazu (snd en)

let dodaj_v_okolje (rez : rezultat) : unit =
    spremenljivo_okolje := {!spremenljivo_okolje with shramba = (!spremenljivo_okolje).shramba @ [rez]};
    let enacba = (match rez with
        | ZacetniRezultat enacba -> enacba
        | NovRezultat (_, enacba) -> enacba
    )
    in
    let neznanka_na_levi_strani = match fst enacba with
        | Neznanka x -> Some x
        | _ -> None
    in
    let neznanke = poisci_neznanke enacba
    in
    let spremenljivke = poisci_spremenljivke enacba
    in
    spremenljivo_okolje := dodaj_sprs spremenljivke;
    spremenljivo_okolje := dodaj_nzns neznanke;
    match neznanka_na_levi_strani with
    | Some x -> (
        spremenljivo_okolje := odstrani_nzn x;
        spremenljivo_okolje := dodaj_spremenljivko !spremenljivo_okolje (from_string_spremenljivka x (Some (snd enacba)))
    )
    | None -> ()


let string_of_seznam_neznank (sez : neznanka list) : string =
    String.concat ", " (List.map Shramba.str_of_neznanka sez)

let string_of_seznam_spremenljivk (sez : spremenljivka list) : string =
    String.concat ", " (List.map Shramba.str_of_spremenljivka sez)

let rec loop () =
  try
    let line = input_line stdin in
      let n = parsaj_input line in (
        match n with 
        | NovaEnacba a -> dodaj_v_okolje (ZacetniRezultat a); print_endline (string_of_int ((List.length (!spremenljivo_okolje.shramba)) - 1))  (* število enačb vrnemo, kolikor jih je v shrambi. to je tudi pointer na našo novo enačbo *)
        | DodajNeznanko ime -> spremenljivo_okolje := dodaj_nzns [ime]; print_endline (string_of_seznam_neznank !spremenljivo_okolje.neznanke)
        | DodajSpremenljivko (ime, vrednost) -> spremenljivo_okolje := dodaj_spremenljivko !spremenljivo_okolje (from_string_spremenljivka ime (Some vrednost))
            ; print_endline (string_of_seznam_spremenljivk !spremenljivo_okolje.spremenljivke)
        | PreoblikujEnacbo (ukaz, originalna_enacba) -> (* ukaz je manipuliraj_enacbo, ki je pač ena sama manipulacija enačbe. originalna_enačba je pointer *)
            let rez = najdi originalna_enacba
            in
            let enacba = (match rez with
                | ZacetniRezultat enacba -> enacba
                | NovRezultat (_, enacba) -> enacba
            )
            in
            let sklep = ManipulirajEnoEnacbo (ukaz, originalna_enacba)
            in 
            let nova_enacba = uporabi enacba ukaz
            in
            dodaj_v_okolje (NovRezultat (sklep, nova_enacba));
            print_endline (string_of_int ((List.length (!spremenljivo_okolje.shramba)) - 1) ^ "?" ^ string_of_seznam_neznank !spremenljivo_okolje.neznanke ^ "?" ^ string_of_seznam_spremenljivk !spremenljivo_okolje.spremenljivke)
        | OperacijaNaEnacbah (ukaz, enacba_1, enacba_2) ->
            let rez1 = najdi enacba_1
            in
            let rez2 = najdi enacba_2
            in
            let ena1 = (match rez1 with
                | ZacetniRezultat enacba -> enacba
                | NovRezultat (_, enacba) -> enacba
            )
            in
            let ena2 = (match rez2 with
                | ZacetniRezultat enacba -> enacba
                | NovRezultat (_, enacba) -> enacba
            )
            in
            let sklep = Operacija (ukaz, enacba_1, enacba_2)
            in 
            let nova_enacba = uporabi_op ena1 ena2 ukaz
            in
            dodaj_v_okolje (NovRezultat (sklep, nova_enacba));
            print_endline (string_of_int ((List.length (!spremenljivo_okolje.shramba)) - 1) ^ "?" ^ string_of_seznam_neznank !spremenljivo_okolje.neznanke ^ "?" ^ string_of_seznam_spremenljivk !spremenljivo_okolje.spremenljivke)
      )
      ;
    flush stdout;
    loop ()
  with
  | End_of_file -> ()
  | Sys_error _ -> ()

let () = loop ()
