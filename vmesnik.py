import tkinter as tk
import subprocess
import os

base = os.path.dirname(os.path.abspath(__file__))
exe_path = os.path.join(base, "program.exe")


COLORS = {
    "novo": "#ffdddd",
    "transformacija": "#ddffdd",
    "operacija": "#ffe5cc",
    "označene_novo": "#ddeeff",
    "označene_transformacija": "#eeddff",
    "označene_operacija": "#eedddd"
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


        # tk.Button(btns, text="Poenostavi", command=self.novi_blok, bg="#9999ff").pack(side="left", padx=5)
        tk.Button(btns, text="Faktoriziraj", command=self.novi_blok2, bg="#9999ff").pack(side="left", padx=5)


        

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
    
        
    def novi_blok2(self):
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

        blok["widget"].bind("<Return>", lambda e, b=blok: self.dodaj_enačbo2(b))
        blok["frame"].bind("<Button-1>", lambda e, b=blok: self.označi_blok(b))
        blok["widget"].bind("<Button-1>", lambda e, b=blok: self.označi_blok(b))

        self.bloki.append(blok)

    def dodaj_enačbo(self, blok):
        enačba = blok["widget"].get()
        blok["widget"].destroy()

        label = tk.Label(blok["frame"], text=enačba, bg=COLORS[blok["tip"]], anchor="w")
        label.pack(padx=5, pady=5)

        label.bind("<Button-1>", lambda e, b=blok: self.označi_blok(b))

        label2 = tk.Label(blok["frame"], text=enačba, bg=COLORS[blok["tip"]], anchor="w")
        label2.pack(padx=5, pady=5)

        label2.bind("<Button-1>", lambda e, b=blok: self.označi_blok(b))

        blok["widget"] = label
        blok["enačba"] = enačba

        blok["frame"].config(bg=COLORS[blok['tip']])
        blok["widget"].config(bg=COLORS[blok['tip']])

        self.poenostavi(enačba, label2)



    def dodaj_enačbo2(self, blok):
        enačba = blok["widget"].get()
        blok["widget"].destroy()

        label = tk.Label(blok["frame"], text=enačba, bg=COLORS[blok["tip"]], anchor="w")
        label.pack(padx=5, pady=5)

        label.bind("<Button-1>", lambda e, b=blok: self.označi_blok(b))

        label2 = tk.Label(blok["frame"], text=enačba, bg=COLORS[blok["tip"]], anchor="w")
        label2.pack(padx=5, pady=5)

        label2.bind("<Button-1>", lambda e, b=blok: self.označi_blok(b))

        blok["widget"] = label
        blok["enačba"] = enačba

        blok["frame"].config(bg=COLORS[blok['tip']])
        blok["widget"].config(bg=COLORS[blok['tip']])

        self.faktorizacija(enačba, label2)
        

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


    
    
    def poenostavi(self, enačba, lbl):
        
        self.proc.stdin.write(f"P {enačba}\n")  # poenostavitev
        self.proc.stdin.flush()
        
        def aux():
            line = self.proc.stdout.readline()
            if line:
                print("OCaml output:", line.strip())
                self.ocaml_vrednost = line.strip()
                lbl["text"] = self.ocaml_vrednost
            else:
                print("ni outputa iz OCamla:", str(line))

        root.after(10, aux)
        

    def faktorizacija(self, enačba, lbl):
        
        self.proc.stdin.write(f"F {enačba}\n")  # faktorizacija
        self.proc.stdin.flush()
        
        def aux():
            line = self.proc.stdout.readline()
            if line:
                print("OCaml output:", line.strip())
                self.ocaml_vrednost = line.strip()
                lbl["text"] = self.ocaml_vrednost
            else:
                print("ni outputa iz OCamla:", str(line))

        root.after(10, aux)
        


        # root.after(100, self.preberi_ocaml_output)
        #self.ocaml_vrednost = line.strip()
        # return line.strip()  # return nič ne doseže. rajš shrani vrednost v nek atribut
        #print("nova enačba:", str(self.ocaml_vrednost))
    

root = tk.Tk()
root.configure(bg="#dddddd")  # okoli gumbov barva
app = Aplikacija(root)
root.protocol("WM_DELETE_WINDOW", app.on_close)
root.mainloop()
