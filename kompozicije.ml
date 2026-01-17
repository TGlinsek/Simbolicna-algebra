type izraz

type enacba = izraz * izraz



(* ------------- kombiniranje algebraičnih manipulacij -------------- *)


type preoblikuj =  (* različni načini, kako preoblikovati nek izraz. Teh bo veliko *)
    | Faktoriziraj
    | Izpostavi
    | ZamenjajVrstniRed of int (* recimo da int predstavlja, kateri člen premakneš na začetek. To je samo primer *)
(* nekateri osnovni ukazi bojo dost splošni, npr. komutiraj tako da sinusi na desni, ali pa naredi nekaj vsem eksponentnim funkcijam, ki so prosto v tem izrazu*)
(* lahko imamo if stavke glede na obliko izrazov. pač, itak vedno vrne ekvivalenten izraz *)



type transformacija =  
(* vsaka funkcija, ki sprejme izraz in vrne nek nov izraz, ampak konsistentno *)
(* ne rabi bit invertibilna, mora pa bit sestavljena iz osnovnih gradnikov, kr pač tisti bojo konsistentni na izrazih z isto vrednostjo *)
(* f je tipa izraz -> izraz, ofc *)
(* npr. če f(x) = x + 1, ampak f(2x - x) = 2x - x + 1, je to v redu. Tudi če f(2x - x) = x + 1, je v redu*)
(* ampak če pa ni konsistentno, potem pa f pač ni ustrezna transformacija *)




type operacija (* vsaka funkcija, ki sprejme dva izraza in vrne neko kombinacijo, npr. vsoto izrazov *)
(* bistveno je, da so to funkcije, ki na dveh izrazih, ki predstavljata isto stvar, spet vrne isto stvar *)
(* glej komentar pri tipu za transformacijo *)


type predikat  (* trasnformacija, ki vrne truth value. ni odvisna od oblike rezultata. torej, ekvivalentna glede na vse ekvivalentne izraze *)


type manipuliraj_izraz =
    | Identiteta
    | Preoblikuj of preoblikuj * manipuliraj_izraz

type manipuliraj_enačbo =
    | Identiteta
    | PreoblikujLevoStran of manipuliraj_izraz * manipuliraj_enačbo
    | PreoblikujDesnoStran of manipuliraj_izraz * manipuliraj_enačbo
    | Transformacija of transformacija * manipuliraj_enačbo  (* aplicira transformacijo na vsaki strani enačbe *)

type sklep = (* enačbo sprejme, ali pa dve; enačbo vrne *)
    | ManipulirajEnoEnačbo of manipuliraj_enačbo * pointer
    | Operacija of operacija * pointer * pointer (* isto kot transformacija, le da sprejme dva izraza *) 

type resnicnostna_vrednost = predikat * pointer (* tk ko transformacija, sam da vrne truth value *)

type rezultat =  (* to je konkreten field, konkretna enačba. eni izhajajo iz ene enačbe (transformacija), ene iz dveh (operacija), eni so že na začetku. iz njih lahko kaj sledi (lahko tudi več), lahko pa nič in je to končni rezultat. *)
    | ZačetniRezultat of enacba
    | NovRezultat of sklep
    | RezultatIfTrue of sklep * resnicnostna_vrednost  (* če resnicnostna_vrednost true, uporabimo na dobljeni enačbi "sklep", če ne samo identito *)
    | RezultatIfFalse of sklep * resnicnostna_vrednost  (* če resnicnostna_vrednost false, uporabimo na dobljeni enačbi "sklep", če ne samo identiteto *)

