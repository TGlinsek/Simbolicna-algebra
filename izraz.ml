open Racionalno

type stevilo =
    (*| Int of int*)
    | Rat of Racionalno.racionalno
    | Dec of float  (* realno število, ali pa neko izmerjeno število ... Lahko je racionalno, ampak ta lastnost ni bistvena *)
    | Spremenljivka of string  (* parameter, konstanta (pi), nedoločenka *)

type simple_izraz =  (* taki ki nimajo transcendentnih funkcij, zato kr je težko reševat enačbe z njimi *)
    | Stevilo of stevilo
    | Plus of simple_izraz * simple_izraz
    | Minus of simple_izraz * simple_izraz
    | Times of simple_izraz * simple_izraz
    | Div of simple_izraz * simple_izraz

type izraz = (* lahko so notri spremenljivke, ampak v izrazu od spremenljivke so lahko samo že definirane spremenljivke *)
    | Stevilo of stevilo
    | Neznanka of string  (* sj se obnaša kot spremenljivka, le da pač omejimo, kje se lahko pojavi v izrazih *)
    | Plus of izraz * izraz
    | Minus of izraz * izraz
    | Times of izraz * izraz
    | Div of izraz * izraz
    | Root of int * stevilo
    | Pow of int * izraz
    (*
    | Sin of simple_izraz
    | Cos of simple_izraz
    | Tan of simple_izraz
    | Exp of simple_izraz
    | Ln of simple_izraz
    *)

let rec najdi_neznanke_v_izrazu (i : izraz) : string list =
    match i with
    | Stevilo x -> []
    | Neznanka x -> [x]
    | Plus (a, b) -> najdi_neznanke_v_izrazu a @ najdi_neznanke_v_izrazu b
    | Minus (a, b) -> najdi_neznanke_v_izrazu a @ najdi_neznanke_v_izrazu b
    | Times (a, b) -> najdi_neznanke_v_izrazu a @ najdi_neznanke_v_izrazu b
    | Div (a, b) -> najdi_neznanke_v_izrazu a @ najdi_neznanke_v_izrazu b
    | Pow (_, b) -> najdi_neznanke_v_izrazu b
    | Root _ -> []

let rec najdi_spremenljivke_v_izrazu (i : izraz) : string list =
    match i with
    | Stevilo x -> (
        match x with
        | Spremenljivka x -> [x]
        | _ -> []
    )
    | Neznanka x -> []
    | Plus (a, b) -> najdi_spremenljivke_v_izrazu a @ najdi_spremenljivke_v_izrazu b
    | Minus (a, b) -> najdi_spremenljivke_v_izrazu a @ najdi_spremenljivke_v_izrazu b
    | Times (a, b) -> najdi_spremenljivke_v_izrazu a @ najdi_spremenljivke_v_izrazu b
    | Div (a, b) -> najdi_spremenljivke_v_izrazu a @ najdi_spremenljivke_v_izrazu b
    | Pow (_, b) -> najdi_spremenljivke_v_izrazu b
    | Root _ -> []




let rat (m : int) (n : int) : izraz =
    Stevilo (Rat {stevec = m; imenovalec = n})

let dec (m : float) : izraz =
    Stevilo (Dec m)

let spr (s : string) : izraz =
    Stevilo (Spremenljivka s)

let ( ++ ) (i : izraz) (j : izraz) : izraz =
    Plus (i, j)
let ( -- ) (i : izraz) (j : izraz) : izraz =
    Minus (i, j)


let ( ** ) (i : izraz) (j : izraz) : izraz =
    Times (i, j)
let ( // ) (i : izraz) (j : izraz) : izraz =
    Div (i, j)

let get_1 (i : izraz) : izraz =
    match i with
    | Plus (j, k) -> j
    | Minus (j, k) -> j
    | Times (j, k) -> j
    | Div (j, k) -> j
    | _ -> failwith "Nima dveh podizrazov"

let get_2 (i : izraz) : izraz =
    match i with
    | Plus (j, k) -> k
    | Minus (j, k) -> k
    | Times (j, k) -> k
    | Div (j, k) -> k
    | _ -> failwith "Nima dveh podizrazov"


let set_1 (i : izraz) (j : izraz) : izraz =
    match i with
    | Plus (k, l) -> Plus (j, l)
    | Times (k, l) -> Times (j, l)
    | Minus (k, l) -> Minus (j, l)
    | Div (k, l) -> Div (j, l)
    | _ -> failwith "Nima dveh podizrazov"


let set_2 (i : izraz) (j : izraz) : izraz =
    match i with
    | Plus (k, l) -> Plus (k, j)
    | Times (k, l) -> Times (k, j)
    | Minus (k, l) -> Minus (k, j)
    | Div (k, l) -> Div (k, j)
    | _ -> failwith "Nima dveh podizrazov"

let set_12 (i : izraz) (j : izraz) (k : izraz) : izraz =
    set_2 (set_1 i j) k

let get_12 (i : izraz) : izraz * izraz =
    (get_1 i, get_2 i)



let rec uporabi_pow (i : izraz) : izraz =
    match i with
    | Pow (k, iz) when k > 0 -> iz ** (uporabi_pow (Pow (k - 1, iz)))
    | Pow (k, iz) when k = 0 -> iz
    | Pow _ -> failwith "Negativen eksponent"
    | _ -> i

let rec zmnozi (k : izraz) (sez : ('a * izraz) list) : ('a * izraz) list =
    match sez with
    | [] -> []
    | (n, i) :: xs -> (n, k ** i) :: zmnozi k xs

let rec aux (sez) =
    match sez with
    | [] -> []
    | ((a, b), (c, d)) :: xs -> ((a +%+ c), b ** d) :: aux xs

let rec konvolucija (sez1 : ('a * izraz) list) (sez2 : ('a * izraz) list) : ('a * izraz) list =
    aux (List.combine sez1 sez2)

let rec vsebuje (i : izraz) (s : string) =
    match i with
    | Plus (x, y) -> vsebuje x s || vsebuje y s
    | Times (x, y) -> vsebuje x s || vsebuje y s
    | Minus (x, y) -> vsebuje x s || vsebuje y s
    | Div (x, y) -> vsebuje x s || vsebuje y s
    | Stevilo x -> (match x with
        | Spremenljivka y -> if y = s then true else false
        | _ -> false)
    | Neznanka x -> if x = s then true else false
    | Root (n, iz) -> (match iz with
        | Spremenljivka y -> if y = s then true else false
        | _ -> false)
    | Pow (n, iz) -> vsebuje iz s

let rec vrni_koeficiente_neznanke (i : izraz) (nedolocenka : string) : ('a * izraz) list =
    let poenostavi = uporabi_pow i
    in
    match poenostavi with
    | Plus (x, y) -> vrni_koeficiente_neznanke x nedolocenka @ vrni_koeficiente_neznanke y nedolocenka
    | Minus (x, y) -> vrni_koeficiente_neznanke x nedolocenka @ (zmnozi (Stevilo (Rat (minus_rat (int_v_rat 1)))) (vrni_koeficiente_neznanke y nedolocenka))
    | Times (x, y) -> konvolucija (vrni_koeficiente_neznanke x nedolocenka) (vrni_koeficiente_neznanke y nedolocenka)
    | Div (x, y) -> if vsebuje y nedolocenka then zmnozi (Div (Stevilo (Rat (int_v_rat 1)), y)) (vrni_koeficiente_neznanke x nedolocenka)
        else failwith "Izraz ni polinom"
    | Pow _ -> failwith "Smo že poenostavili pow"
    | Root _ -> [(int_v_rat 0, poenostavi)]
    | Stevilo _ -> [(int_v_rat 0, poenostavi)]
    | Neznanka x -> if x = nedolocenka then [(int_v_rat 1, rat 1 1)] else [(int_v_rat 0, poenostavi)]

let rec vrni_koeficiente_spremenljivke (i : izraz) (nedolocenka : string) : ('a * izraz) list =
    let poenostavi = uporabi_pow i
    in
    match poenostavi with
    | Plus (x, y) -> vrni_koeficiente_spremenljivke x nedolocenka @ vrni_koeficiente_spremenljivke y nedolocenka
    | Minus (x, y) -> vrni_koeficiente_spremenljivke x nedolocenka @ (zmnozi (Stevilo (Rat (minus_rat (int_v_rat 1)))) (vrni_koeficiente_spremenljivke y nedolocenka))
    | Times (x, y) -> konvolucija (vrni_koeficiente_spremenljivke x nedolocenka) (vrni_koeficiente_spremenljivke y nedolocenka)
    | Div (x, y) -> if vsebuje y nedolocenka then zmnozi (Div (Stevilo (Rat (int_v_rat 1)), y)) (vrni_koeficiente_spremenljivke x nedolocenka)
        else failwith "Izraz ni polinom"
    | Pow _ -> failwith "Smo že poenostavili pow"
    | Root (n, st) -> (match st with
        | Spremenljivka x -> if x = nedolocenka then [({stevec = 1; imenovalec = n}, rat 1 1)] else [(int_v_rat 0, poenostavi)]
        | _ -> [(int_v_rat 0, poenostavi)]
    )
    | Stevilo st -> (match st with
        | Spremenljivka x -> if x = nedolocenka then [(int_v_rat 1, rat 1 1)] else [(int_v_rat 0, poenostavi)]
        | _ -> [(int_v_rat 0, poenostavi)]
    )
    | Neznanka x -> [(int_v_rat 0, poenostavi)]

let izraz1 = rat 2 1 ++ rat 3 1 ** rat 4 1 
let izraz2 = rat 3 1 ** rat 4 1 ++ rat 2 1

let sin = izraz1 // izraz2 
let sinex = izraz1 -- izraz2

let stevilo_to_string (k : stevilo) : string =
    match k with
    | Rat x -> izpisi_racionalno x
    | Dec x -> Float.to_string x
    | Spremenljivka x -> x

let izraz_to_string (k : izraz) : string =
    let rec aux (i : izraz) : string = 
        match i with
        | Plus (x, y) -> "(" ^ aux' x ^ " + " ^ aux' y ^ ")"
        | Minus (x, y) -> "(" ^ aux' x ^ " + " ^ aux' y ^ ")"
        | Times (x, y) -> aux x ^ " * " ^ aux y
        | Div (x, y) -> aux x ^ " / (" ^ aux' y ^ ")"
        | Pow (n, x) -> "(" ^ aux' x ^ ")^" ^ Int.to_string n
        | Root (n, x) -> "(" ^ stevilo_to_string x ^ ")^" ^ "(1/" ^ Int.to_string n ^ ")"
        | Neznanka x -> x
        | Stevilo x -> stevilo_to_string x

    and aux' (j : izraz) : string = 
        match j with
        | Plus (x, y) -> aux' x ^ " + " ^ aux' y
        | Minus (x, y) -> aux' x ^ " - " ^ aux y
        | Times (x, y) -> aux x ^ " * " ^ aux y
        | Div (x, y) -> aux x ^ " / " ^ aux y
        | Pow (n, x) -> "(" ^ aux' x ^ ")^" ^ Int.to_string n
        | Root (n, x) -> "(" ^ stevilo_to_string x ^ ")^" ^ "(1/" ^ Int.to_string n ^ ")"
        | Neznanka x -> x
        | Stevilo x -> stevilo_to_string x
    in
    aux' k

let rati = Neznanka "abc"


let rec bottoms_up (f : izraz -> izraz) (i : izraz) : izraz =
    match i with
    | Stevilo x -> f (Stevilo x)
    | Neznanka x -> f (Neznanka x)
    | Root (x, y) -> f (Root (x, y))
    | Pow (x, y) -> f (Pow (x, bottoms_up f y))
    | Plus (x, y) -> f (Plus (bottoms_up f x, bottoms_up f y))
    | Minus (x, y) -> f (Minus (bottoms_up f x, bottoms_up f y))
    | Times (x, y) -> f (Times (bottoms_up f x, bottoms_up f y))
    | Div (x, y) -> f (Div (bottoms_up f x, bottoms_up f y))



let rec spravi_div_zunaj (i : izraz) : izraz =  (* popravi dvojne ulomke ipd. *)
    match i with
    | Plus (x', y') -> (
        let x = spravi_div_zunaj x'
        in
        let y = spravi_div_zunaj y'
        in
        match (x, y) with
        | Div (a, b), Div (c, d) -> Div (Plus (Times (a, d), Times (b, c)), b ** d)
        | Div (a, b), s -> Div (Plus (a, Times (b, s)), b)
        | s, Div (a, b) -> Div (Plus (Times (b, s), a), b)
        | _, _ -> i
    )
    | Minus (x', y') -> (
        let x = spravi_div_zunaj x'
        in
        let y = spravi_div_zunaj y'
        in
        match (x, y) with
        | Div (a, b), Div (c, d) -> Div (Minus (Times (a, d), Times (b, c)), b ** d)
        | Div (a, b), s -> Div (Minus (a, Times (b, s)), b)
        | s, Div (a, b) -> Div (Minus (Times (b, s), a), b)
        | _, _ -> i
    )
    | Times (x', y') -> (
        let x = spravi_div_zunaj x'
        in
        let y = spravi_div_zunaj y'
        in
        match (x, y) with
        | Div (a, b), Div (c, d) -> Div (a ** c, b ** d)
        | Div (a, b), s -> Div (a ** s, b)
        | s, Div (a, b) -> Div (s ** a, b)
        | _, _ -> i
    )
    | Div (x', y') -> (
        let x = spravi_div_zunaj x'
        in
        let y = spravi_div_zunaj y'
        in
        match (x, y) with
        | Div (a, b), Div (c, d) -> Div (a ** d, b ** c)
        | Div (a, b), s -> Div (a, b ** s)
        | s, Div (a, b) -> Div (a, s ** b)
        | _, _ -> i
    )
    | Root _ -> i
    | Pow (k, izr) -> (
        let iz = spravi_div_zunaj izr
        in
        match iz with
        | Div (a, b) -> Div (Pow (k, a), Pow (k, b))
        | _ -> iz
    )
    | Stevilo _ -> i
    | Neznanka _ -> i


let rec distribute (i : izraz) : izraz =
    match i with
    | Times (x, y) -> 
        (
            match x with
            | Plus (a, b) -> bottoms_up distribute (Plus (Times (a, y), Times (b, y)))  (* damo še bottoms up, ne samo distribute, zato da gre še enkrat čez vse, v primeru da se je kaj spremenilo. verjetno bi en sam nivo nižje bilo dovolj, ampak *)
            | Minus (a, b) -> bottoms_up distribute (Minus (Times (a, y), Times (b, y)))
            | _ -> (
                match y with
                    | Plus (a, b) -> bottoms_up distribute (Plus (Times (x, a), Times (x, b)))
                    | Minus (a, b) -> bottoms_up distribute (Minus (Times (x, a), Times (x, b)))
                    | _ -> i
            )
        )
    | Div (x, y) -> (
        (
            match x with
            | Plus (a, b) -> bottoms_up distribute (Plus (Div (a, y), Div (b, y)))
            | Minus (a, b) -> bottoms_up distribute (Minus (Div (a, y), Div (b, y)))
            | _ -> i
        )
    )
    | _ -> i

let distribute_polno = bottoms_up distribute

let rec asociiraj (i : izraz) : izraz = 
    match i with
    | Times (Times (x, y), z) -> bottoms_up asociiraj (Times (x, Times (y, z)))
    | Times (Div (x, y), z) -> bottoms_up asociiraj (Times (x, Div (z, y)))
    | Plus (Plus (x, y), z) -> bottoms_up asociiraj (Plus (x, Plus (y, z)))
    | Plus (Minus (x, y), z) -> bottoms_up asociiraj (Plus (x, Minus (z, y)))
    | Minus (Plus (x, y), z) -> bottoms_up asociiraj (Plus (x, Minus (y, z)))
    | Minus (x, Minus (y, z)) -> bottoms_up asociiraj (Plus (x, Minus (z, y)))
    | Minus (x, Plus (y, z)) -> bottoms_up asociiraj (Minus (Minus (x, y), z))
    | _ -> i

let asociiraj_polno = bottoms_up asociiraj

(*
let menjaj_vrstni red
(* lahko določimo, katero neznanko damo na kero stran pa kak je vrstni red, to je pa to *)
*)

let rec vsebuje_neznanko (i : izraz) =
    match i with
    | Plus (x, y) -> vsebuje_neznanko x || vsebuje_neznanko y
    | Minus (x, y) -> vsebuje_neznanko x || vsebuje_neznanko y
    | Times (x, y) -> vsebuje_neznanko x || vsebuje_neznanko y
    | Div (x, y) -> vsebuje_neznanko x || vsebuje_neznanko y
    | Root (x, y) -> vsebuje_neznanko (Stevilo y)
    | Pow (x, y) -> vsebuje_neznanko y
    | Stevilo _ -> false
    | Neznanka _ -> true


(*
let rec člen_z_neznanko (i : izraz) =
    match i with
    | Plus (a, b) -> let (x, y) = člen_z_neznanko a
        in
        let (z, w) = člen_z_neznanko b
        in
        (Plus (x, z), Plus (y, w))
    | Times (a, b) -> (
        if vsebuje_neznanko a then

    )

let neznanke_na_levo (i : izraz) : izraz =
    match i with
    | Plus (a, b) -> 
        match 
*)


