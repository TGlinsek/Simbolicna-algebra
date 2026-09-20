open Racionalno
open Izraz
open Solver
open Parser
open Metode


let testni_primeri_poenostavitev = [
    "1 + 2 * 3 + 5";
    "1 + (2 * y + 6) * (5 + x + 3)";
    "(x - 1) * (x * x + x + 1)"
]


(* število n nam pove, na katerem intervalu [-n, n] naj išče koeficiente faktorizacije *)
let testni_primeri_faktorizacija = [
    ("x * x - y * y", 4);
    ("x * 4 * x * x * x - 64", 4);
    ("x*x - y*y + z*x + y*z", 4);
    ("x*x*x*x*x*x*x*x - 256", 16);
    ("10*x+25*x*x+30*x*x*x", 16);
    ("(5 * x - 2) * (4 * x + 3)", 5);
    ("(5 * x - 2) * (4 * y + 3)", 5);
]


let () = print_endline "Poenostavitve:"; flush stdout

let izpisi_poenostavitev : unit =
    let rec aux seznam_izrazov =
        match seznam_izrazov with
        | [] -> ()
        | i :: is -> 
            i |> parsaj_v_izraz |> poenostavitev |> izraz_to_string |> print_endline;
            flush stdout;
            aux is
    in
    aux testni_primeri_poenostavitev


let () = print_endline "\nFaktorizacije:"; flush stdout

let izpisi_faktorizacijo : unit =
    let rec aux seznam_izrazov =
        match seznam_izrazov with
        | [] -> ()
        | (i, n) :: is -> 
            n |> (i |> parsaj_v_izraz |> poenostavitev |> faktorizacija) |> fst |> izraz_to_string |> print_endline;
            flush stdout;
            aux is
    in
    aux testni_primeri_faktorizacija
