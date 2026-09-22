open Izraz
open Metode
open Parser
open Nastavitve



let substring_do_konca (i : string) (n : int) =
    String.sub i n (String.length i - n)



let preberi_input line i : string =
    match Char.uppercase_ascii (String.sub line 0 1).[0] with
    | 'F' -> (
        let input = i
        in
        let (faktorizirano, uspelo) = faktorizacija (input |> parsaj_v_izraz) zgornja_meja
        in
        (
            if uspelo then
                "Faktorizacija: "
            else
                "Ni bila najdena faktorizacija: "
        ) ^ (faktorizirano |> izraz_to_string)
    )
    | 'P' -> (
        let input = i
        in
        let izraz = input |> parsaj_v_izraz
        in
        "Poenostavitev: " ^ (izraz |> poenostavitev |> izraz_to_string)
    )
    | _ -> failwith "Neveljaven parameter"
