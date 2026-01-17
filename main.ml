

type inp = (* input iz vmesnika *)
    | NovaEnacba of enacba
    | PreoblikujEnacbo of manipuliraj_enacbo * pointer
    | OperacijaNaEnacbah of operacija * pointer * pointer
    

let parsaj_input (i : string) : inp =
    failwith ""

let najdi (a : pointer) : rezultat = 
    let rec aux sez (i : pointer) : rezultat =
        match (sez, i) with
        | [], _ -> failwith "prazen seznam"
        | x :: _, 0 -> x
        | x :: xs, i when i > 0 -> aux xs (i - 1)
        | _ -> failwith "Napaka, slab indeks"
    in
    aux (!spremenljivo_okolje).shramba a

let uporabi (enacba : enacba) (ukaz : manipuliraj_enacbo) : enacba = 
    failwith ""

let uporabi_op (ena1 : enacba) (ena2 : enacba) (ukaz : operacija) : enacba = 
    failwith ""

let poisci_neznanke (en : enacba) : string list = failwith ""
let poisci_spremenljivke (en : enacba) : string list = failwith ""

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

let () =
  try
    while true do
      let line = read_line () in
      let n = parsaj_input line in
        match n with 
        | NovaEnacba a -> dodaj_v_okolje (ZacetniRezultat a); print_endline "ustvarili smo novo enačbo"
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
            print_endline "uporabili smo preoblikovanje enačbe"
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
            print_endline "uporabili smo operacijo"
      ;
      flush stdout
    done
  with End_of_file -> ()

