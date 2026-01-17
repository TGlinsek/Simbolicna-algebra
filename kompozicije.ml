type izraz

type enacba = izraz * izraz



(* ------------- kombiniranje algebraičnih manipulacij -------------- *)


type preoblikuj =  (* različni načini, kako preoblikovati nek izraz. Teh bo veliko *)
    | Faktoriziraj
    | Izpostavi
    | ZamenjajVrstniRed of int (* recimo da int predstavlja, kateri člen premakneš na začetek. To je samo primer *)
(* nekateri osnovni ukazi bojo dost splošni, npr. komutiraj tako da sinusi na desni, ali pa naredi nekaj vsem eksponentnim funkcijam, ki so prosto v tem izrazu*)
(* lahko imamo if stavke glede na obliko izrazov. pač, itak vedno vrne ekvivalenten izraz *)



type transformacija 
(* vsaka funkcija, ki sprejme izraz in vrne nek nov izraz, ampak konsistentno *)
(* ne rabi bit invertibilna, mora pa bit sestavljena iz osnovnih gradnikov, kr pač tisti bojo konsistentni na izrazih z isto vrednostjo *)
(* f je tipa izraz -> izraz, ofc *)
(* npr. če f(x) = x + 1, ampak f(2x - x) = 2x - x + 1, je to v redu. Tudi če f(2x - x) = x + 1, je v redu*)
(* ampak če pa ni konsistentno, potem pa f pač ni ustrezna transformacija *)




type operacija (* vsaka funkcija, ki sprejme dva izraza in vrne neko kombinacijo, npr. vsoto izrazov *)
(* bistveno je, da so to funkcije, ki na dveh izrazih, ki predstavljata isto stvar, spet vrne isto stvar *)
(* glej komentar pri tipu za transformacijo *)





type manipuliraj_izraz =
    | Identiteta
    | Preoblikuj of preoblikuj * manipuliraj_izraz

type manipuliraj_enacbo =
    | Identiteta
    | PreoblikujLevoStran of manipuliraj_izraz
    | PreoblikujDesnoStran of manipuliraj_izraz
    | Transformacija of transformacija  (* aplicira transformacijo na vsaki strani enačbe *)

type sklep = (* enačbo sprejme, ali pa dve; enačbo vrne *)
    | ManipulirajEnoEnacbo of manipuliraj_enacbo * pointer
    | Operacija of operacija * pointer * pointer (* isto kot transformacija, le da sprejme dva izraza *) 

type rezultat =  (* to je konkreten field, konkretna enačba. eni izhajajo iz ene enačbe (transformacija), ene iz dveh (operacija), eni so že na začetku. iz njih lahko kaj sledi (lahko tudi več), lahko pa nič in je to končni rezultat. *)
    | ZacetniRezultat of enacba
    | NovRezultat of sklep * enacba  (* dodali smo tu enačbo, ker pač ne bomo vsakič šli celotnega postopka računanja novih izrazov. se pa da iz sklepa samega dobit to enačbo, če gremo vse še enkrat poračunat. *)
