open Racionalno
open Izraz
open Solver
open Parser
open Metode



let substring_do_konca (i : string) (n : int) =
    String.sub i n (String.length i - n)


let rec loop () =
  try
    let line = input_line stdin in
    match String.sub line 0 2 with
    | "F " -> (
        let input = substring_do_konca line 2
        in
        let faktorja = main_funkcija (input |> parsaj_v_izraz)
        in
        match faktorja with
            | None -> print_endline "Ni bila najdena faktorizacija"
            | Some (x, y) -> (
                "Faktorizacija: " ^ (Times (x, y) |> poenostavi_izraz_polno |> izraz_to_string) |> print_endline
            )
    )
    | "P " -> (
        let input = substring_do_konca line 2
        in
        let izraz = input |> parsaj_v_izraz |> distribute_polno 
        in
        "Poenostavitev: " ^ (izraz |> poenostavi_izraz_polno |> izraz_to_string) |> print_endline
    )
    | _ -> failwith "Neveljaven parameter";
    ;
    flush stdout;
    loop ()
  with
  | End_of_file -> ()
  | Sys_error _ -> ()

let () = loop ()
