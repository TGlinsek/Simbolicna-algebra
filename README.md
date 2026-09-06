# Simbolicna-algebra
Projektna naloga pri predmetu Matematika z računalnikom, 2025/26.

## Opis projekta

Pri reševanju problemov v matematiki je velikokrat potrebno "iti za nosom", tj. računati brez pretirane uporabe intuicije. To je veščina, ki se jo začnemo učiti že zelo zgodaj v naši matematični izobrazbi - za mnoge ljudi takšno računanje ostane kar sinonim za matematiko kot vedo.

Matematika pa je vseeno več kot le to, zato bi si marsikateri matematik verjetno želel prihraniti čas tako, da bi brezglavo računanje prepustil algoritmom, seveda v kolikor algoritem za to obstaja. S takimi algoritmi se ukvarja veja simbolične algebre; obstaja že precej programov, ki znajo simbolično računati in s tem prihraniti matematiku kar nekaj dela, npr. Wolfram Mathematica.

# Cilj projekta

Ker bi dela res bilo preveč za vse, so implementirane osnovne metode za manipuliranje izrazov in pa algoritem za iskanje faktorizacije polinomov. Dodan je tudi python vmesnik, na katerem pa ni bilo poudarka, zato je ostal precej skromen.

# Datoteke

- racionalno.ml - v njem je definiran tip racionalnih števil
- izraz.ml - v njem je definiran tip matematičnega izraza, skupaj z raznimi metodami
- solver.ml - metode za iskanje faktorizacije polinoma (tudi polinomov z več spremenljivkami)
- parser.ml - osnovni parser za izraze
- metode.ml - nabor metod, ki povezujejo ostale: metode za prevajanje polinoma v tip za monome, tj. člene oblike x_1^a_1 * x_2^a_2 * ... * x_n^a_n
- main.ml - glavne metode, ki uporabijo še parser in se povežejo s python vmesnikom
- vmesnik.py - GUI vmesnik, narejen s pythonovo knjižnico Tkinter
