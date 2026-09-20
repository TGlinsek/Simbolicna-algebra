open Racionalno

type izraz =
    | Rat of racionalno
    | Neznanka of string
    | Plus of izraz * izraz
    | Minus of izraz * izraz
    | Times of izraz * izraz
    | Div of izraz * izraz
    | Root of int * izraz
    | Pow of int * izraz

let rec najdi_neznanke_v_izrazu (i : izraz) : string list =
    match i with
    | Rat x -> []
    | Neznanka x -> [x]
    | Plus (a, b) -> najdi_neznanke_v_izrazu a @ najdi_neznanke_v_izrazu b
    | Minus (a, b) -> najdi_neznanke_v_izrazu a @ najdi_neznanke_v_izrazu b
    | Times (a, b) -> najdi_neznanke_v_izrazu a @ najdi_neznanke_v_izrazu b
    | Div (a, b) -> najdi_neznanke_v_izrazu a @ najdi_neznanke_v_izrazu b
    | Pow (_, b) -> najdi_neznanke_v_izrazu b
    | Root (_, b) -> najdi_neznanke_v_izrazu b



let rat (m : int) (n : int) : izraz =
    (Rat {stevec = m; imenovalec = n})

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
    | Rat x -> false
    | Neznanka x -> if x = s then true else false
    | Root (n, iz) -> vsebuje iz s
    | Pow (n, iz) -> vsebuje iz s

let rec vrni_koeficiente_neznanke (i : izraz) (nedolocenka : string) : ('a * izraz) list =
    let poenostavi = uporabi_pow i
    in
    match poenostavi with
    | Plus (x, y) -> vrni_koeficiente_neznanke x nedolocenka @ vrni_koeficiente_neznanke y nedolocenka
    | Minus (x, y) -> vrni_koeficiente_neznanke x nedolocenka @ (zmnozi (Rat (minus_rat (int_v_rat 1))) (vrni_koeficiente_neznanke y nedolocenka))
    | Times (x, y) -> konvolucija (vrni_koeficiente_neznanke x nedolocenka) (vrni_koeficiente_neznanke y nedolocenka)
    | Div (x, y) -> if vsebuje y nedolocenka then zmnozi (Div (Rat (int_v_rat 1), y)) (vrni_koeficiente_neznanke x nedolocenka)
        else failwith "Izraz ni polinom"
    | Pow _ -> failwith "Smo že poenostavili pow"
    | Root _ -> [(int_v_rat 0, poenostavi)]
    | Rat _ -> [(int_v_rat 0, poenostavi)]
    | Neznanka x -> if x = nedolocenka then [(int_v_rat 1, rat 1 1)] else [(int_v_rat 0, poenostavi)]


let izraz1 = rat 2 1 ++ rat 3 1 ** rat 4 1
let izraz2 = rat 3 1 ** rat 4 1 ++ rat 2 1

let sin = izraz1 // izraz2 
let sinex = izraz1 -- izraz2

let izraz_to_string (k : izraz) : string =
    let rec aux (i : izraz) : string = 
        match i with
        | Plus (x, y) -> "(" ^ aux' x ^ " + " ^ aux' y ^ ")"
        | Minus (x, y) -> "(" ^ aux' x ^ " - " ^ aux y ^ ")"
        | Times (x, y) -> aux x ^ " * " ^ aux y
        | Div (x, y) -> aux x ^ " / (" ^ aux' y ^ ")"
        | Pow (n, x) -> "(" ^ aux' x ^ ")^" ^ Int.to_string n
        | Root (n, x) -> "(" ^ aux' x ^ ")^" ^ "(1/" ^ Int.to_string n ^ ")"
        | Neznanka x -> x
        | Rat x when x <%< (int_v_rat 0) -> "(" ^ izpisi_racionalno x ^ ")"
        | Rat x -> izpisi_racionalno x
    and aux' (j : izraz) : string = 
        match j with
        | Plus (x, y) -> aux' x ^ " + " ^ aux' y
        | Minus (x, y) -> aux' x ^ " - " ^ aux y
        | Times (x, y) -> aux x ^ " * " ^ aux y
        | Div (x, y) -> aux x ^ " / (" ^ aux' y ^ ")"
        | Pow (n, x) -> "(" ^ aux' x ^ ")^" ^ Int.to_string n
        | Root (n, x) -> "(" ^ aux' x ^ ")^" ^ "(1/" ^ Int.to_string n ^ ")"
        | Neznanka x -> x
        | Rat x when x <%< (int_v_rat 0) -> "(" ^ izpisi_racionalno x ^ ")"
        | Rat x -> izpisi_racionalno x
    in
    aux' k

let rati = Neznanka "abc"


let rec bottoms_up (f : izraz -> izraz) (i : izraz) : izraz =
    match i with
    | Rat x -> f (Rat x)
    | Neznanka x -> f (Neznanka x)
    | Root (x, y) -> f (Root (x, bottoms_up f y))
    | Pow (x, y) -> f (Pow (x, bottoms_up f y))
    | Plus (x, y) -> f (Plus (bottoms_up f x, bottoms_up f y))
    | Minus (x, y) -> f (Minus (bottoms_up f x, bottoms_up f y))
    | Times (x, y) -> f (Times (bottoms_up f x, bottoms_up f y))
    | Div (x, y) -> f (Div (bottoms_up f x, bottoms_up f y))



let rec spravi_div_zunaj (i : izraz) : izraz =  (* popravi dvojne ulomke ipd. *)
    match i with
    | Plus (x', y') -> (
        let x = bottoms_up spravi_div_zunaj x'
        in
        let y = bottoms_up spravi_div_zunaj y'
        in
        match (x, y) with
        | Div (a, b), Div (c, d) -> Div (Plus (Times (a, d), Times (b, c)), b ** d)
        | Div (a, b), s -> Div (Plus (a, Times (b, s)), b)
        | s, Div (a, b) -> Div (Plus (Times (b, s), a), b)
        | _, _ -> i
    )
    | Minus (x', y') -> (
        let x = bottoms_up spravi_div_zunaj x'
        in
        let y = bottoms_up spravi_div_zunaj y'
        in
        match (x, y) with
        | Div (a, b), Div (c, d) -> Div (Minus (Times (a, d), Times (b, c)), b ** d)
        | Div (a, b), s -> Div (Minus (a, Times (b, s)), b)
        | s, Div (a, b) -> Div (Minus (Times (b, s), a), b)
        | _, _ -> i
    )
    | Times (x', y') -> (
        let x = bottoms_up spravi_div_zunaj x'
        in
        let y = bottoms_up spravi_div_zunaj y'
        in
        match (x, y) with
        | Div (a, b), Div (c, d) -> Div (a ** c, b ** d)
        | Div (a, b), s -> Div (a ** s, b)
        | s, Div (a, b) -> Div (s ** a, b)
        | _, _ -> i
    )
    | Div (x', y') -> (
        let x = bottoms_up spravi_div_zunaj x'
        in
        let y = bottoms_up spravi_div_zunaj y'
        in
        match (x, y) with
        | Div (a, b), Div (c, d) -> Div (a ** d, b ** c)
        | Div (a, b), s -> Div (a, b ** s)
        | s, Div (a, b) -> Div (a, s ** b)
        | _, _ -> i
    )
    | Root _ -> i
    | Pow (k, izr) -> (
        let iz = bottoms_up spravi_div_zunaj izr
        in
        match iz with
        | Div (a, b) -> Div (Pow (k, a), Pow (k, b))
        | _ -> iz
    )
    | Rat _ -> i
    | Neznanka _ -> i


let spravi_div_zunaj_polno = bottoms_up spravi_div_zunaj

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

let poenostavi_aux (i : izraz) : izraz = 
    (* operacije na enotah se trivialno poenostavijo *)
    match i with
    | Plus (Rat r, b) when r =%= (int_v_rat 0) -> b
    | Plus (b, Rat r) when r =%= (int_v_rat 0) -> b
    
    | Times (Rat r, b) when r =%= (int_v_rat 0) -> rat 0 1
    | Times (b, Rat r) when r =%= (int_v_rat 0) -> rat 0 1
    | Times (Rat r, b) when r =%= (int_v_rat 1) -> b
    | Times (b, Rat r) when r =%= (int_v_rat 1) -> b

    | Div (Rat r, b) when r =%= (int_v_rat 0) -> rat 0 1
    | Div (b, Rat r) when r =%= (int_v_rat 1) -> b

    (* izračunaj izračunljivo *)
    | Plus (Rat r, Rat s) -> Rat (r +%+ s)
    | Minus (Rat r, Rat s) -> Rat (r -%- s)
    | Times (Rat r, Rat s) -> Rat (r *%* s)
    | Div (Rat r, Rat s) -> Rat (r /%/ s)

    | Minus (Rat r, b) when r =%= (int_v_rat 0) -> Times (Rat (int_v_rat (-1)), b)
    | Minus (b, Rat r) when r =%= (int_v_rat 0) -> b
    | _ -> i

let rec poenostavi_izraz (i : izraz) : izraz = 
    (* poenostavi notranje izraze, nato pa vse distribuiraj *)
    match i with
    | Plus (a, b) -> poenostavi_aux (Plus (bottoms_up poenostavi_izraz a, bottoms_up poenostavi_izraz b)) |> distribute_polno
    | Times (a, b) -> poenostavi_aux (Times (bottoms_up poenostavi_izraz a, bottoms_up poenostavi_izraz b)) |> distribute_polno
    | Minus (a, b) -> poenostavi_aux (Minus (bottoms_up poenostavi_izraz a, bottoms_up poenostavi_izraz b)) |> distribute_polno
    | Div (a, b) -> poenostavi_aux (Div (bottoms_up poenostavi_izraz a, bottoms_up poenostavi_izraz b)) |> distribute_polno
    | Pow (n, b) -> poenostavi_aux (Pow (n, bottoms_up poenostavi_izraz b)) |> distribute_polno
    | Root (n, b) -> poenostavi_aux (Root (n, bottoms_up poenostavi_izraz b)) |> distribute_polno
    | _ -> i

let poenostavi_izraz_polno = bottoms_up poenostavi_izraz

let rec komutiraj_koeficiente (i : izraz) : izraz =
    (* spravi koeficiente na levo, da jih lahko potem poračunamo med sabo *)
    match i with
    (* primeri, kjer imamo dve znani vrednosti (izmed treh) *)
    | Times (Rat x, Times (Rat y, c)) -> Times (Rat (x *%* y), c)
    | Times (Rat x, Times (c, Rat y)) -> Times (Rat (x *%* y), c)
    | Times (Times (Rat y, c), Rat x) -> Times (Rat (x *%* y), c)
    | Times (Times (c, Rat y), Rat x) -> Times (Rat (x *%* y), c)
    | Times (Rat x, Div (Rat y, c)) -> Div (Rat (x *%* y), c)
    | Times (Rat x, Div (c, Rat y)) -> Times (Rat (x /%/ y), c)
    | Times (Div (Rat y, b), Rat x) -> Div (Rat (x *%* y), b)
    | Times (Div (b, Rat y), Rat x) -> Times (Rat (x /%/ y), b)
    | Div (Times (Rat x, b), Rat y) -> Times (Rat (x /%/ y), b)
    | Div (Times (b, Rat x), Rat y) -> Times (Rat (x /%/ y), b)
    | Div (Rat x, Times (Rat y, c)) -> Div (Rat (x /%/ y), c)
    | Div (Rat x, Times (c, Rat y)) -> Div (Rat (x /%/ y), c)

    | Plus (Rat x, Plus (Rat y, c)) -> Plus (Rat (x +%+ y), c)
    | Plus (Rat x, Plus (b, Rat y)) -> Plus (Rat (x +%+ y), b)
    | Plus (Plus (Rat y, b), Rat x) -> Plus (Rat (x +%+ y), b)
    | Plus (Plus (a, Rat y), Rat x) -> Plus (Rat (x +%+ y), a)
    | Plus (Rat x, Minus (Rat y, c)) -> Minus (Rat (x +%+ y), c)
    | Plus (Rat x, Minus (c, Rat y)) -> Plus (Rat (x -%- y), c)
    | Plus (Minus (Rat y, c), Rat x) -> Minus (Rat (x +%+ y), c)
    | Plus (Minus (c, Rat y), Rat x) -> Plus (Rat (x -%- y), c)
    | Minus (Rat x, Plus (Rat y, c)) -> Minus (Rat (x -%- y), c)
    | Minus (Rat x, Plus (b, Rat y)) -> Minus (Rat (x -%- y), b)
    | Minus (Plus (Rat y, b), Rat x) -> Plus (Rat (y -%- x), b)
    | Minus (Plus (a, Rat y), Rat x) -> Plus (Rat (y -%- x), a)
    | Minus (Rat x, Minus (Rat y, c)) -> Plus (Rat (x -%- y), c)
    | Minus (Rat x, Minus (b, Rat y)) -> Minus (Rat (x +%+ y), b)
    | Minus (Minus (Rat y, b), Rat x) -> Minus (Rat (y -%- x), b)
    | Minus (Minus (a, Rat y), Rat x) -> Plus (Rat (minus_rat (x +%+ y)), a)


    (* če poznamo samo eno vrednost izmed treh, lahko velikokrat še vedno kak koeficient damo na levo*)
    | Plus (a, Plus (Rat x, c)) -> Plus (Rat x, Plus (a, c))
    | Plus (a, Plus (c, Rat x)) -> Plus (Rat x, Plus (a, c))
    | Times (a, Times (Rat x, c)) -> Times (Rat x, Times (a, c))
    | Times (a, Times (c, Rat x)) -> Times (Rat x, Times (a, c))
    | Times (a, Div (Rat x, c)) -> Times (Rat x, Div (a, c))

    | Times (Neznanka a, Neznanka b) -> (
        if compare a b < 0 then
            Times (Neznanka a, Neznanka b)
        else 
            Times (Neznanka b, Neznanka a)
        )  (* spravi spremenljivke v členu v nek smiseln vrstni red*)
    
    (* poračunaj, če poznamo obe vrednosti *)
    | Plus (Rat r, Rat s) -> Rat (r +%+ s)
    | Minus (Rat r, Rat s) -> Rat (r -%- s)
    | Times (Rat r, Rat s) -> Rat (r *%* s)
    | Div (Rat r, Rat s) -> Rat (r /%/ s)

    (* spravi koeficient na levo *)
    | Times (a, Rat x) -> Times (Rat x, a)
    | Plus (a, Rat x) -> Plus (Rat x, a)

    (* minus kar pretvori v plus, da je čim manj zmede *)
    | Minus (a, b) -> Plus (a, Times (Rat (int_v_rat (-1)), b)) |> komutiraj_koeficiente
    | _ -> i

let komutiraj_koeficiente_polno = bottoms_up komutiraj_koeficiente

let rec asociiraj (i : izraz) : izraz = 
    (* asociativnost *)
    match i with
    | Times (Times (x, y), z) -> bottoms_up asociiraj (Times (x, Times (y, z)))
    | Times (Div (x, y), z) -> bottoms_up asociiraj (Times (x, Div (z, y)))
    | Plus (Plus (x, y), z) -> bottoms_up asociiraj (Plus (x, Plus (y, z)))
    | Plus (Minus (x, y), z) -> bottoms_up asociiraj (Plus (x, Minus (z, y)))
    | Minus (Plus (x, y), z) -> bottoms_up asociiraj (Plus (x, Minus (y, z)))
    | Minus (x, Minus (y, z)) -> bottoms_up asociiraj (Plus (x, Minus (z, y)))
    (* | Minus (x, Plus (y, z)) -> bottoms_up asociiraj (Minus (Minus (x, y), z))*)  (* mogoče raje ne kreiraj dodatnih minusov *)
    | _ -> i

let asociiraj_polno = bottoms_up asociiraj


let rec vsebuje_neznanko (i : izraz) =
    match i with
    | Plus (x, y) -> vsebuje_neznanko x || vsebuje_neznanko y
    | Minus (x, y) -> vsebuje_neznanko x || vsebuje_neznanko y
    | Times (x, y) -> vsebuje_neznanko x || vsebuje_neznanko y
    | Div (x, y) -> vsebuje_neznanko x || vsebuje_neznanko y
    | Root (x, y) -> vsebuje_neznanko y
    | Pow (x, y) -> vsebuje_neznanko y
    | Rat _ -> false
    | Neznanka _ -> true


let rec poracunaj_stevilski_izraz (i : izraz) : racionalno =
    match i with
    | Rat r -> r
    | Plus (a, b) -> (poracunaj_stevilski_izraz a) +%+ (poracunaj_stevilski_izraz b)
    | Minus (a, b) -> (poracunaj_stevilski_izraz a) -%- (poracunaj_stevilski_izraz b)
    | Times (a, b) -> (poracunaj_stevilski_izraz a) *%* (poracunaj_stevilski_izraz b)
    | Div (a, b) -> (poracunaj_stevilski_izraz a) /%/ (poracunaj_stevilski_izraz b)
    | _ -> failwith "Ni število"
