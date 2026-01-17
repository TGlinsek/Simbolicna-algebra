

type izraz  (* lahko so notri spremenljivke, ampak v izrazu od spremenljivke so lahko samo že definirane spremenljivke *)

type spremenljivka = { label : string; vrednost : izraz option }
type neznanka = { label : string }

type stevilo =
    | Int of int
    | Rat of int
    | Dec of float  (* realno število, ali pa neko izmerjeno število ... Lahko je racionalno, ampak ta lastnost ni bistvena *)
    | Spremenljivka of string  (* parameter, konstanta (pi), nedoločenka *)

type simple_izraz =  (* taki ki nimajo transcendentnih funkcij, zato kr je težko reševat enačbe z njimi *)
    | Stevilo of stevilo
    | Plus of simple_izraz * simple_izraz
    | Minus of simple_izraz * simple_izraz
    | Times of simple_izraz * simple_izraz
    | Div of simple_izraz * simple_izraz

type izraz = 
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
