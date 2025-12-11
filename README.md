# Simbolicna-algebra
Projektna naloga pri predmetu Matematika z računalnikom, 2025/26.

## Opis projekta

Pri reševanju problemov v matematiki je velikokrat potrebno "iti za nosom", tj. računati brez pretirane uporabe intuicije. To je veščina, ki se jo začnemo učiti že zelo zgodaj v naši matematični izobrazbi - za mnoge ljudi takšno računanje ostane kar sinonim za matematiko kot vedo.

Matematika pa je vseeno več kot le to, zato bi si marsikateri matematik verjetno želel prihraniti čas tako, da bi brezglavo računanje prepustil algoritmom, seveda v kolikor algoritem za to obstaja. S takimi algoritmi se ukvarja veja simbolične algebre; obstaja že precej programov, ki znajo simbolično računati in s tem prihraniti matematiku kar nekaj dela, npr. Wolfram Mathematica.

Nekaj podobnega, a veliko bolj rudimentarnega bom poskusil ustvariti pri tem projektu. Izdelati želim program z uporabniškim vmesnikom, kjer bo uporabnik lahko vstavil svoj problem (ali je to izraz, ki ga je treba poenostaviti (razstaviti, faktorizirati, kaj izpostaviti, ...), ali pa enačba/sistem enačb, ki ga je potrebno rešiti), program pa mu bo izpisal postopek, kako to narediti. Velikokrat je takih postopkov več, zato bomo uporabljali kriterij, ki določi, kateri postopek uporabiti in predstaviti uporabniku.

## Cilj projekta

Cilj je, da algoritmi, ki jih uporabljamo za algebraično manipulacijo, niso le hevristike, ampak dejanski deterministični algoritmi. Algoritmi morajo izraz/enačbo poenostaviti v neko kanonično obliko, to pa morajo znati za vsak element iz izbrane družine izrazov/enačb. Če torej algoritem ne zna poenostaviti izraza do konca, potem sporočimo uporabniku, da tega izraza ni bilo mogoče poenostaviti do želene oblike.

Naši algoritmi bodo ne le vrnili poenostavljen izraz, ampak tudi postopek, kako smo do te poenostavitve prišli. Ta postopek bomo tudi lepo vizualizirali, da bo razumljiv uporabniku. Postopki naj bodo na nivoju osnovne oz. srednje šole (že tako ali tako lahko algoritmi postanejo pretežki, če jih poskušamo napisati za npr. trigonometrične enačbe; te so tudi snov srednje šole).
