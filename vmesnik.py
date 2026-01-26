import tkinter as tk
import subprocess
import os
# import time

base = os.path.dirname(os.path.abspath(__file__))
exe_path = os.path.join(base, "vmesnik.exe")


COLORS = {
    "novo": "#ffdddd",
    "transformacija": "#ddffdd",
    "operacija": "#ffe5cc",
    "označene_novo": "#ddeeff",
    "označene_transformacija": "#eeddff",
    "označene_operacija": "#eedddd"
}

PREOBLIKUJ = {
    "distribute" : "A",
    "na_ulomek" : "B",
    "na_minus" : "C",
    "obrni" : "D",
    "neznanke" : "E"
}

OPERACIJE = {
    "seštej" : "+",
    "odštej" : "-",
    "pomnoži" : "X",
    "deli" : "/"
}


class Aplikacija:
    def __init__(self, root):
        self.root = root
        root.title("Simbolična algebra")

        self.ocaml_vrednost = None

        self.bloki = []
        self.označene = []
        self.operacija_mode = False
        self.operacija = None

        # Buttons
        btns = tk.Frame(root, bg="#ccccff")  # barva okoli gumbov (tist pas)
        btns.pack(pady=10, fill="x")

        
        tk.Button(btns, text="Nova enačba", command=self.novi_blok, bg="#9999ff").pack(side="left", padx=5)

        self.bottom_label = tk.Label(
            btns,
            text="PREOBLIKUJ:",
            anchor="w"
        )
        self.bottom_label.pack(side="left", padx=10)

        tk.Button(btns, text=PREOBLIKUJ["distribute"], command=lambda : self.transformacija("distribute")).pack(side="left", padx=0)
        tk.Button(btns, text=PREOBLIKUJ["na_ulomek"], command=lambda : self.transformacija("na_ulomek")).pack(side="left", padx=0)
        tk.Button(btns, text=PREOBLIKUJ["na_minus"], command=lambda : self.transformacija("na_minus")).pack(side="left", padx=0)
        tk.Button(btns, text=PREOBLIKUJ["obrni"], command=lambda : self.transformacija("obrni")).pack(side="left", padx=0)
        tk.Button(btns, text=PREOBLIKUJ["neznanke"], command=lambda : self.transformacija("na_levo")).pack(side="left", padx=0)
        
        self.bottom_label = tk.Label(
            btns,
            text="OPERACIJE:",
            anchor="w"
        )
        self.bottom_label.pack(side="left", padx=10)

        tk.Button(btns, text=OPERACIJE["seštej"], command=lambda : self.enter_operacija_mode("seštej")).pack(side="left", padx=0)
        tk.Button(btns, text=OPERACIJE["odštej"], command=lambda : self.enter_operacija_mode("odštej")).pack(side="left", padx=0)
        tk.Button(btns, text=OPERACIJE["pomnoži"], command=lambda : self.enter_operacija_mode("pomnoži")).pack(side="left", padx=0)
        tk.Button(btns, text=OPERACIJE["deli"], command=lambda : self.enter_operacija_mode("deli")).pack(side="left", padx=0)

        tk.Button(btns, text="Izbriši", command=self.izbriši).pack(side="left", padx=15)

        # self.container = tk.Frame(root)
        # self.container.pack(anchor="w", padx=10)  # pred scrollanje smo tk mel
        self.main_frame = tk.Frame(root)
        self.main_frame.pack(fill="both", expand=True)
        

        # zgornji del okna
        self.top_frame = tk.Frame(self.main_frame)
        self.top_frame.pack(fill="both", expand=True)
        
        # levi del
        self.canvas_frame = tk.Frame(self.top_frame)
        self.canvas_frame.pack(side="left", fill="both", expand=True)
        # self.canvas_frame.pack(fill="both", expand=True)  # staro, rabimo met side=left

        # desni del
        self.vlist_frame = tk.Frame(self.top_frame, width=200)
        self.vlist_frame.pack(side="right", fill="y")

        # seznam spremenljivk
        self.spremenljivke = []

        # gumbi za spremenljivke
        btns2 = tk.Frame(self.vlist_frame)
        btns2.pack(pady=10)

        tk.Button(btns2, text="Add", command=lambda : self.dodaj_spremenljivko("neki", 123)).pack(side="left", padx=5)
        tk.Button(btns2, text="Delete", command=self.izbriši_spremenljivko).pack(side="left", padx=5)

        # okvir za seznam spremenljivk
        self.okvir_za_seznam_spremenljivk = tk.Frame(self.vlist_frame)
        self.okvir_za_seznam_spremenljivk.pack(padx=10, pady=10, anchor="w")

        self.označena_spremenljivka = None


        # canvas
        ozadje = "#f0fddd"
        self.canvas = tk.Canvas(self.canvas_frame, highlightthickness=0, bg=ozadje)
        self.scrollbar = tk.Scrollbar(self.canvas_frame, orient="vertical", command=self.canvas.yview)
        self.canvas.configure(yscrollcommand=self.scrollbar.set)
        self.scrollbar.config(width=16)
        self.canvas.pack(side="left", fill="both", expand=True)

        self.scrollbar.pack(side="right", fill="y")
        
        self.canvas.bind("<Configure>", self._on_resize)

        self.container = tk.Frame(self.canvas, bg=ozadje)
        self.canvas.create_window((0, 0), window=self.container, anchor="nw")

        self.container.bind(
            "<Configure>",
            lambda e: self.canvas.configure(
                scrollregion=self.canvas.bbox("all")
            )
        )
        def _on_mousewheel(event):
            self.canvas.yview_scroll(-1 * (event.delta // 120), "units")

        self.canvas.bind_all("<MouseWheel>", _on_mousewheel)


        # neznanke so spodaj
        self.spodaj = tk.Frame(self.main_frame, bg="#abcdef")
        self.spodaj.pack(side="bottom", fill="x", pady=5)

        self.bottom_label = tk.Label(
            self.spodaj,
            text="Neznanke:",
            anchor="w"
        )
        self.bottom_label.pack(side="left", padx=10)

        self.bottom_entries = []
        self.add_bottom_entry("X")
        self.add_bottom_entry("Y")

        self.bottom_entries_frame = tk.Frame(self.spodaj)
        self.bottom_entries_frame.pack(side="left")

        # poveži se z ocaml programom
        self.proc = subprocess.Popen(
            [exe_path],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,  # pusti to, kr pomembno
            bufsize=1
        )

    def _on_resize(self, event):  # to ne vem zakaj rabimo. ne nardi nobene opazne razlike
        self.canvas.configure(scrollregion=self.canvas.bbox("all"))

    # dodaj neznanko spodaj
    def add_bottom_entry(self, črka):  # samo python dodaja neznanke, ali pa user. ocaml jih ne
        frame = tk.Frame(self.spodaj)
        frame.pack(side="left", padx=5)

        var = tk.StringVar(value=črka)
        entry = tk.Label(frame, textvariable=var, width=3, justify="center")
        entry.pack()

        entry_data = {
            "frame": frame,
            "entry": entry,
            "var": var
        }

        # ko pritisnemo Enter, shranimo v seznam
        entry.bind("<Return>", lambda e, d=entry_data: self.shrani_neznanko(d))

        self.bottom_entries.append(entry_data)

    def shrani_neznanko(self, entry_data):
        value = entry_data["var"].get()
        self.send_letter_to_ocaml(value)

    # ustvari blok/enačbo
    def novi_blok(self):
        frame = tk.Frame(self.container, bd=1, bg=COLORS["novo"], relief="solid")
        frame.pack(anchor="w", pady=5, padx=10)
        
        entry = tk.Entry(frame, width=30)
        entry.pack(padx=5, pady=5)

        blok = {
            "frame": frame,
            "widget": entry,
            "enačba": None,
            "tip": "novo",
            "x_pos": 10,
            "ocaml_position" : None
        }

        blok["widget"].bind("<Return>", lambda e, b=blok: self.dodaj_enačbo(b))
        blok["frame"].bind("<Button-1>", lambda e, b=blok: self.označi_blok(b))
        blok["widget"].bind("<Button-1>", lambda e, b=blok: self.označi_blok(b))

        self.bloki.append(blok)
        

    def dodaj_enačbo(self, blok):
        enačba = blok["widget"].get()
        blok["widget"].destroy()

        label = tk.Label(blok["frame"], text=enačba, bg=COLORS[blok["tip"]], anchor="w")
        label.pack(padx=5, pady=5)

        label.bind("<Button-1>", lambda e, b=blok: self.označi_blok(b))

        blok["widget"] = label
        blok["enačba"] = enačba

        blok["frame"].config(bg=COLORS[blok['tip']])
        blok["widget"].config(bg=COLORS[blok['tip']])

        if blok["ocaml_position"] is None:
            blok["ocaml_position"] = self.dodaj_v_ocamlu(enačba)

        

    # označi
    def označi_blok(self, blok):
        if self.operacija_mode:
            self.označene.append(blok)
            self.obarvaj(blok)

            if len(self.označene) == 2:
                self.ustvari_blok_z_operacijo()
        else:
            self.odznači()
            self.označene = [blok]
            self.obarvaj(blok)

    def obarvaj(self, blok):
        blok["frame"].config(bg=COLORS[f"označene_{blok['tip']}"])
        blok["widget"].config(bg=COLORS[f"označene_{blok['tip']}"])
        

    def odznači(self):
        for b in self.bloki:
            color = COLORS[b["tip"]]
            b["frame"].config(bg=color)
            b["widget"].config(bg=color)
        self.označene = []

    # transformacija
    def transformacija(self, ukaz):
        if len(self.označene) != 1:
            return

        označeni_bloki = self.označene[0]
        if označeni_bloki["enačba"] is None:
            return

        idx = self.bloki.index(označeni_bloki)
        

        nova_enačba = self.poračunaj(označeni_bloki["enačba"], ukaz)
        if nova_enačba == označeni_bloki["enačba"]:
            print("isto")

        frame = tk.Frame(self.container, bg=COLORS["transformacija"], bd=1, relief="solid")
        # frame.pack(anchor="w", pady=5, padx=20)
        
        label = tk.Label(frame, text=nova_enačba, bg=COLORS["transformacija"], anchor="w")
        label.pack(padx=5, pady=5)

        frame.pack(
            anchor="w",
            pady=5,
            padx=označeni_bloki["x_pos"] + 15,
            after=označeni_bloki["frame"]
        )

        blok = {
            "frame": frame,
            "widget": label,
            "enačba": nova_enačba,
            "tip": "transformacija",
            "x_pos": označeni_bloki["x_pos"] + 15
        }

        blok["widget"].bind("<Button-1>", lambda e, b=blok: self.označi_blok(b))
        blok["frame"].bind("<Button-1>", lambda e, b=blok: self.označi_blok(b))

        self.bloki.insert(idx + 1, blok)
        self.označi_blok(blok)

    # operacija
    def enter_operacija_mode(self, ukaz):
        self.operacija = ukaz
        self.odznači()
        self.operacija_mode = True

    def ustvari_blok_z_operacijo(self):
        b1, b2 = self.označene
        enačba1 = b1["enačba"]
        enačba2 = b2["enačba"]

        if enačba1 is None or enačba2 is None:
            self.operacija_mode = False
            return


        nova_enačba = self.pridobi_rezultat_ukaza(enačba1, enačba2, self.operacija)

        frame = tk.Frame(self.container, bg=COLORS["operacija"], bd=1, relief="solid")
        frame.pack(anchor="w", pady=5, padx=10)

        label = tk.Label(frame, text=nova_enačba, bg=COLORS["operacija"], anchor="w")
        label.pack(padx=5, pady=5)

        blok = {
            "frame": frame,
            "widget": label,
            "enačba": nova_enačba,
            "tip": "operacija",
            "x_pos" : 10
        }

        blok["widget"].bind("<Button-1>", lambda e, b=blok: self.označi_blok(b))
        blok["frame"].bind("<Button-1>", lambda e, b=blok: self.označi_blok(b))

        self.bloki.append(blok)
        
        
        self.operacija_mode = False
        # self.odznači()
        self.označi_blok(blok)  # bomo raje kar imeli selectano

    # brisanje enačbe
    def izbriši(self):
        if len(self.označene) != 1:
            return

        b = self.označene[0]
        b["frame"].destroy()
        self.bloki.remove(b)
        self.odznači()

    # seznam spremenljivk
    # Dodaj vrstico
    def dodaj_spremenljivko(self, letter, v):
        frame = tk.Frame(self.okvir_za_seznam_spremenljivk)
        frame.pack(anchor="w", pady=2)

        lbl = tk.Label(frame, text=letter, width=3, anchor="w")
        lbl.pack(side="left")

        vrednost = tk.StringVar(value=v)
        
        entry = tk.Entry(frame, textvariable=vrednost, width=10)
        entry.pack(side="left", padx=5)

        row = {
            "frame": frame,
            "letter": lbl,
            "entry": entry,
            "vrednost" : vrednost
        }

        # Selection bindings
        frame.bind("<Button-1>", lambda e, r=row: self.označi_spremenljivko(r))
        lbl.bind("<Button-1>", lambda e, r=row: self.označi_spremenljivko(r))
        entry.bind("<Button-1>", lambda e, r=row: self.označi_spremenljivko(r))
        entry.bind("<Return>", lambda e, r=row: self.posodobi_vrednost(r))  # Return ali FocusOut

        self.spremenljivke.append(row)
        self.označi_spremenljivko(row)


    def posodobi_vrednost(self, row):
        value = row["vrednost"].get()
        letter = row["letter"].cget("text")

        self.posodobi_ocaml_seznam(letter, value)
        row["entry"].selection_clear()
        self.root.focus()


    # Označi vrstico
    def označi_spremenljivko(self, row):
        if self.označena_spremenljivka:
            self.označena_spremenljivka["frame"].config(bg="white")
            self.označena_spremenljivka["letter"].config(bg="white")
            self.označena_spremenljivka["entry"].config(bg="white")

        self.označena_spremenljivka = row
        row["frame"].config(bg="#ddeeaa")  # najboljš je tem trem dat isto barvo, ddeeff recimo
        row["letter"].config(bg="#ddeecc")
        row["entry"].config(bg="#ddeeff")

    # Izbriši vrstico
    def izbriši_spremenljivko(self):
        if not self.označena_spremenljivka:
            return

        self.označena_spremenljivka["frame"].destroy()
        self.spremenljivke.remove(self.označena_spremenljivka)
        self.označena_spremenljivka = None


    def on_close(self):
        self.proc.terminate()
        self.root.destroy()

    # povezava z ocamlom
    def poračunaj(self, enačba, ukaz):
        if ukaz == "distribute":
            self.proc.stdin.write(f"A {enačba}\n")
        elif ukaz == "na_ulomek":
            self.proc.stdin.write(f"B {enačba}\n")
        elif ukaz == "na_minus":
            self.proc.stdin.write(f"C {enačba}\n")
        elif ukaz == "obrni":
            self.proc.stdin.write(f"D {enačba}\n")
        elif ukaz == "na_levo":
            self.proc.stdin.write(f"E {enačba}\n")
        
        self.proc.stdin.flush()
        root.after(10, self.preberi_ocaml_output)
        print("preoblikovana enačba:", self.ocaml_vrednost)

    def pridobi_rezultat_ukaza(self, enačba1, enačba2, ukaz):
        if ukaz == "seštej":
            self.proc.stdin.write(f"+ {enačba1},{enačba2}\n")
        elif ukaz == "odštej":
            self.proc.stdin.write(f"- {enačba1},{enačba2}\n")
        elif ukaz == "pomnoži":
            self.proc.stdin.write(f"* {enačba1},{enačba2}\n")
        elif ukaz == "deli":
            self.proc.stdin.write(f"/ {enačba1},{enačba2}\n")
        
        self.proc.stdin.flush()
        
        root.after(10, self.preberi_ocaml_output)
        print("rezultat operacije:", self.ocaml_vrednost)
    
    def posodobi_ocaml_seznam(self, črka, vrednost):
        
        self.proc.stdin.write(f"Y {črka},{vrednost}\n")
        self.proc.stdin.flush()

        root.after(10, self.preberi_ocaml_output)
        print("nova spremenljivka:", self.ocaml_vrednost)

    def send_letter_to_ocaml(self, črka):
        self.proc.stdin.write(f"X {črka}\n")
        self.proc.stdin.flush()

        root.after(10, self.preberi_ocaml_output)
        print("nova neznanka:", self.ocaml_vrednost)
    
    def dodaj_v_ocamlu(self, enačba):
        
        print(f"N {enačba}")
        self.proc.stdin.write(f"N {enačba}\n")
        self.proc.stdin.flush()
        
        
        root.after(10, self.preberi_ocaml_output)
        # time.sleep(0.010)
        print("nova enačba:", self.ocaml_vrednost)  # ne dela. očitno se mora ta funkcija najprej do konca izvest, preden se preberi_ocaml_output požene
        

    def preberi_ocaml_output(self):
        line = self.proc.stdout.readline()
        if line:
            print("OCaml output:", line.strip())
        else:
            print("ni outputa iz OCamla:", line)
        
        # root.after(100, self.preberi_ocaml_output)
        self.ocaml_vrednost = line.strip()
        # return line.strip()  # return nič ne doseže. rajš shrani vrednost v nek atribut

    
root = tk.Tk()
root.configure(bg="#dddddd")  # okoli gumbov barva
app = Aplikacija(root)
root.protocol("WM_DELETE_WINDOW", app.on_close)
root.mainloop()
