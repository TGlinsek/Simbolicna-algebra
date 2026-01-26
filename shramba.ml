open Kompozicije

type spremenljivka = { label : string; vrednost : Izraz.izraz option }
type neznanka = { label : string }

let from_string_spremenljivka (label : string) (vrednost : Izraz.izraz option) = { label; vrednost }
let to_string_spremenljivka { label; _ } : string = label
let to_value_spremenljivka { vrednost ; _} : Izraz.izraz option = vrednost

let from_string_neznanka (label : string) = { label }
let to_string_neznanka { label } : string = label





type shramba = rezultat list

let empty_shramba : shramba = []





type okolje = { spremenljivke : spremenljivka list ; neznanke : neznanka list ; shramba : rezultat list }  (* spremenljivke so proste spremenljivke *)

let shrani (o : okolje) (r : rezultat) : okolje * pointer =  (* vrne nov array in pointer novega elementa *)
    ({o with shramba = o.shramba @ [r]}, List.length o.shramba)



let ustvari_okolje (spr : spremenljivka list) (nzn : neznanka list) (s : shramba) = { spremenljivke = spr; neznanke = nzn; shramba = s}
let empty_okolje : okolje = ustvari_okolje [] [] []

let odstrani_neznanko (o : okolje) (nzn : neznanka) = { o with neznanke = List.filter (fun el -> el <> nzn) o.neznanke}

let dodaj_spremenljivko (o : okolje) (spr : spremenljivka) = { o with spremenljivke = spr :: o.spremenljivke}

let dodaj_spremenljivke (o : okolje) (spr : spremenljivka list) = { o with spremenljivke = spr @ o.spremenljivke}

let str_of_spremenljivka (spr : spremenljivka) : string =
    "{" ^ spr.label ^ "|" ^ (
        match spr.vrednost with 
            | Some i -> Izraz.izraz_to_string i 
            | None -> ""
    ) ^ "}"

let dodaj_neznanke (o : okolje) (nzn : neznanka list) = { o with neznanke = nzn @ o.neznanke}

let str_of_neznanka (nzn : neznanka) : string =
    nzn.label


let from_string_spremenljivke (spr : string list) = (List.map (fun x -> from_string_spremenljivka x None) spr)
let from_string_neznanke (spr : string list) = (List.map (fun x -> from_string_neznanka x) spr)


let spremenljivo_okolje : okolje ref = ref empty_okolje

let odstrani_nzn (nzn : string) : okolje = odstrani_neznanko !spremenljivo_okolje (from_string_neznanka nzn)
let dodaj_spr (spr : string) = dodaj_spremenljivko !spremenljivo_okolje (from_string_spremenljivka spr None)
let dodaj_sprs (spr : string list) = dodaj_spremenljivke !spremenljivo_okolje (from_string_spremenljivke spr)
let dodaj_nzns (nzns : string list) : okolje = dodaj_neznanke !spremenljivo_okolje (from_string_neznanke nzns)





