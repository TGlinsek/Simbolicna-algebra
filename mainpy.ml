open Racionalno
open Izraz
open Solver
open Parser
open Metode
open Pomozne
open Vmesnik


let zgornja_meja = 5  (* to je zgornja meja za koeficiente: vsi koeficienti, ki se lahko pojavijo v iskani faktorizaciji, bodo kvečjemu n, po absolutni vrednosti *)

let substring_do_konca (i : string) (n : int) =
    String.sub i n (String.length i - n)


let rec loop () =
  try
    let line = input_line stdin in
    preberi_input line (substring_do_konca line 2) |> print_endline
    ;
    flush stdout;
    loop ()
  with
  | End_of_file -> ()
  | Sys_error _ -> ()

let () = loop ()
