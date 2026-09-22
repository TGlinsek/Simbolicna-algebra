open Racionalno
open Izraz
open Solver
open Parser
open Metode
open Pomozne
open Vmesnik


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
