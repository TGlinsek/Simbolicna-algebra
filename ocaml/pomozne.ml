(* pomožne funkcije na seznamih *)

let rec concat_brez_ponavljanj (xs) (ys) =
    (* predpostavljamo, da sta brez ponavljanj *)
    match ys with
    | [] -> xs
    | y :: ys' -> (
        if List.exists (fun x -> y = x) xs then
            concat_brez_ponavljanj xs ys'
        else
            y :: (concat_brez_ponavljanj (xs) ys')
    )


let rec zamenjaj_pri_indeksu (sez : 'a list) i (f : 'a -> 'a) =
    match i, sez with
    | 0, s :: rest ->
        (f s) :: rest
    | n, x :: rest ->
        x :: zamenjaj_pri_indeksu rest (n - 1) f
    | _, [] ->
        failwith "prevelik indeks"


let rec zdruzi_seznama (a : ('a * 'b) list) (b : ('a * 'b) list) (enakost : 'a -> 'a -> bool) (g : 'b -> 'b -> 'b) =
    match b with
    | [] -> a
    | (x, y) :: xs ->
        match List.find_opt (fun (x', _) -> enakost x x') a with
        | Some (_, _) -> let a' = List.map (
            fun (x'', y'') ->
                if enakost x x'' then
                   (x'', g y y'')
                else
                   (x'', y'')
            ) a
            in
            zdruzi_seznama a' xs enakost g
        | None -> zdruzi_seznama ((x, y) :: a) xs enakost g


let rec seznama_sta_enaka (s1) (s2) : bool =
    match s1, s2 with
    | (x :: xs), (y :: ys) -> x = y && seznama_sta_enaka xs ys
    | [], [] -> true
    | _ -> false


let rec find_index (f : 'a -> bool) (sez : 'a list) : int =
    match sez with
    | x :: xs -> if f x then 0 else (find_index f xs) + 1
    | [] -> failwith "Ni takega indeksa"


let rec vsi_elementi_so_enaki (enakost : 'a -> 'a -> bool) (sez : 'a list) : 'a option =  (* vrne vrednost elementa, če so vsi enaki. Če je seznam prazen, vrne None *)
    match sez with
    | [] -> None
    | [x] -> Some x
    | x :: xs -> (
        match vsi_elementi_so_enaki enakost xs with
        | None -> None
        | Some r -> (
            if enakost x r then
                Some r
            else
                None
        )
    )