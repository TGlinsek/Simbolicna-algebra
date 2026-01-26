import tkinter as tk

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

        self.bloki = []
        self.označene = []
        self.operacija_mode = False

        # Buttons
        btns = tk.Frame(root, bg="#ccccff")  # barva okoli gumbov (tist pas)
        btns.pack(pady=10, fill="x")

        tk.Button(btns, text="Nova enačba", command=self.novi_blok, bg="#9999ff").pack(side="left", padx=5)
        tk.Button(btns, text="Transformacija + 1", command=self.transformacija).pack(side="left", padx=5)
        tk.Button(btns, text="Operacija", command=self.enter_operacija_mode).pack(side="left", padx=5)
        tk.Button(btns, text="Izbriši", command=self.izbriši).pack(side="left", padx=5)

        # self.container = tk.Frame(root)
        # self.container.pack(anchor="w", padx=10)  # pred scrollanje smo tk mel


        ozadje = "#f0fddd"
        self.canvas = tk.Canvas(root, highlightthickness=0, bg=ozadje)
        self.scrollbar = tk.Scrollbar(root, orient="vertical", command=self.canvas.yview)
        self.canvas.configure(yscrollcommand=self.scrollbar.set)

        self.scrollbar.pack(side="right", fill="y")
        self.canvas.pack(side="left", fill="both", expand=True)

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


    # ustvari blok/enačbo
    def novi_blok(self):
        frame = tk.Frame(self.container, bd=1, bg=COLORS["novo"], relief="solid")
        frame.pack(anchor="w", pady=5, padx=10)
        
        entry = tk.Entry(frame, width=30)
        entry.pack(padx=5, pady=5)

        blok = {
            "frame": frame,
            "widget": entry,
            "equation": None,
            "kind": "novo",
            "x_pos": 10
        }

        blok["widget"].bind("<Return>", lambda e, b=blok: self.dodaj_enačbo(b))
        blok["frame"].bind("<Button-1>", lambda e, b=blok: self.označi_blok(b))
        blok["widget"].bind("<Button-1>", lambda e, b=blok: self.označi_blok(b))

        self.bloki.append(blok)

    def dodaj_enačbo(self, blok):
        eq = blok["widget"].get()
        blok["widget"].destroy()

        label = tk.Label(blok["frame"], text=eq, bg=COLORS[blok["kind"]], anchor="w")
        label.pack(padx=5, pady=5)

        label.bind("<Button-1>", lambda e, b=blok: self.označi_blok(b))

        blok["widget"] = label
        blok["equation"] = eq

        blok["frame"].config(bg=COLORS[blok['kind']])
        blok["widget"].config(bg=COLORS[blok['kind']])

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
        blok["frame"].config(bg=COLORS[f"označene_{blok['kind']}"])
        blok["widget"].config(bg=COLORS[f"označene_{blok['kind']}"])
        

    def odznači(self):
        for b in self.bloki:
            color = COLORS[b["kind"]]
            b["frame"].config(bg=color)
            b["widget"].config(bg=color)
        self.označene = []

    # transformacija
    def transformacija(self):
        if len(self.označene) != 1:
            return

        označeni_bloki = self.označene[0]
        if označeni_bloki["equation"] is None:
            return

        idx = self.bloki.index(označeni_bloki)
        

        new_eq = označeni_bloki["equation"] + " + 1"

        frame = tk.Frame(self.container, bg=COLORS["transformacija"], bd=1, relief="solid")
        # frame.pack(anchor="w", pady=5, padx=20)
        
        label = tk.Label(frame, text=new_eq, bg=COLORS["transformacija"], anchor="w")
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
            "equation": new_eq,
            "kind": "transformacija",
            "x_pos": označeni_bloki["x_pos"] + 15
        }

        blok["widget"].bind("<Button-1>", lambda e, b=blok: self.označi_blok(b))
        blok["frame"].bind("<Button-1>", lambda e, b=blok: self.označi_blok(b))

        self.bloki.insert(idx + 1, blok)
        self.označi_blok(blok)

    # operacija
    def enter_operacija_mode(self):
        self.odznači()
        self.operacija_mode = True

    def ustvari_blok_z_operacijo(self):
        b1, b2 = self.označene
        eq1 = b1["equation"]
        eq2 = b2["equation"]

        if eq1 is None or eq2 is None:
            self.operacija_mode = False
            return

        new_eq = pridobi_rezultat_ukaza(eq1, eq2)

        frame = tk.Frame(self.container, bg=COLORS["operacija"], bd=1, relief="solid")
        frame.pack(anchor="w", pady=5, padx=10)

        label = tk.Label(frame, text=new_eq, bg=COLORS["operacija"], anchor="w")
        label.pack(padx=5, pady=5)

        blok = {
            "frame": frame,
            "widget": label,
            "equation": new_eq,
            "kind": "operacija",
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


def pridobi_rezultat_ukaza(eq1, eq2):
    # placeholder
    return f"({eq1}) + ({eq2})"


root = tk.Tk()
root.configure(bg="#dddddd")  # okoli gumbov barva
app = Aplikacija(root)
root.mainloop()
