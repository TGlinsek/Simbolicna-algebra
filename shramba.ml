type izraz  (* lahko so notri spremenljivke, ampak v izrazu od spremenljivke so lahko samo že definirane spremenljivke *)

type spremenljivka = { label : string; vrednost : izraz option }
type neznanka = { label : string }

let from_string_spremenljivka (label : string) (vrednost : izraz option) = { label; vrednost }
let to_string_spremenljivka { label; _ } : string = label
let to_value_spremenljivka { vrednost ; _} : izraz option = vrednost

let from_string_neznanka (label : string) = { label }
let to_string_neznanka { label } : string = label




type rezultat
type pointer = int

type shramba = rezultat array

let get (s : shramba) (p : pointer) : rezultat =
    s.(p)

let empty_shramba : shramba = [||]





type okolje = { spremenljivke : spremenljivka list ; neznanke : neznanka list ; shramba : rezultat array }  (* spremenljivke so proste spremenljivke *)

let shrani (o : okolje) (r : rezultat) : okolje * pointer =  (* vrne nov array in pointer novega elementa *)
    ({o with shramba = Array.append o.shramba [|r|]}, Array.length o.shramba)



let ustvari_okolje (spr : spremenljivka list) (nzn : neznanka list) (s : shramba) = { spremenljivke = spr; neznanke = nzn; shramba = s}
let empty : okolje = ustvari_okolje [] [] [||]

let odstrani_neznanko (o : okolje) (nzn : neznanka) = { o with neznanke = List.filter (fun el -> el <> nzn) o.neznanke}

let dodaj_spremenljivko (o : okolje) (spr : spremenljivka)= { o with spremenljivke = spr :: o.spremenljivke}






