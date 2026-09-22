open Racionalno
open Izraz
open Solver
open Parser
open Metode
open Pomozne
open Vmesnik


let preberi_input2 line i : string =
    try
        let a = preberi_input line i
        in
        print_endline a;
        flush stdout;
        a
    with
    | Failure msg -> if msg = "Neveljaven parameter" then
            line
        else failwith msg
  
let main () =
    print_endline "Vnesi izraz:";
    flush stdout;
    let izbor = input_line stdin in
    print_endline ("Kaj želiš storiti z izrazom? Vnesi F (za faktorizacijo) ali P (za poenostavitev):");
    flush stdout;
    let prebran = input_line stdin in

    let nov_izraz = preberi_input prebran izbor
    in
    print_endline nov_izraz;
    flush stdout;
    
    let rec loop nov = (
        let izrazek = 
            if String.exists (fun c -> c = ':') nov then
                substring_do_konca nov ((String.index_from nov 0 ':') + 1)
            else
                nov
        in
        print_endline ("Kaj želiš storiti z izrazom? Vnesi F (za faktorizacijo) ali P (za poenostavitev), sicer pa vnesi nov izraz:");
        flush stdout;
        let prebran2 = input_line stdin
        in
        let nov_izraz2 = preberi_input2 prebran2 izrazek
        in
        loop nov_izraz2
    )
    in
    loop nov_izraz

let () = main ()