module Rac = struct
    type racionalno = {stevec : int; imenovalec : int}


    let sign (n : int) : int =
        if n > 0 then 1
        else
            if n < 0 then -1
            else 0

    let max_abs (a : int) (b : int) : int =
        if Int.abs a > Int.abs b then
            a
        else
            b

    let min_abs (a : int) (b : int) : int =
        if Int.abs a < Int.abs b then
            a
        else
            b

    let rec gcd (a : int) (b : int) : int =  (* gcd2 *)
        (* vrne tak predznak kot a (razen če a == 0, potem vrne kar b) *)
        if a = 0 || b = 0 then
            max_abs a b
        else
            let aux = (Int.abs b) * (sign a)
            in
            if a mod b = 0 then
                aux
            else
                gcd aux (a - aux)


    let rec gcd_extended (a : int) (b : int) : int * int * int = 
        if a = 0 || b = 0 then
            (max_abs a b,
            (if Int.abs a > Int.abs b then 1 else 0),
            (if Int.abs b > Int.abs a then 1 else 0))
        else
            if a mod b = 0 then
                (b, 1, 1 - a/b)
            else
                let kontrola = Int.abs a >= Int.abs b
                in
                let a' = max_abs a b in
                let b' = min_abs a b
                in
                let (gcd, n, m) = gcd_extended b' ((sign b') * (Int.abs a') - b') 
                in
                if kontrola then
                    if gcd != m * sign a * sign b * a + (n - m) * b then failwith "neki"
                    else
                        (gcd, m * sign a * sign b, n - m)
                else
                    if gcd != (n - m) * a + (m * sign a * sign b) * b then failwith "neki2"
                    else
                        (gcd, n - m, m * sign a * sign b)

    let kanonicno_racionalno (r : racionalno) : racionalno =  (* vedno je imenovalec rezultata pozitivno število *)
        let d = gcd r.imenovalec r.stevec
        in
        {stevec = r.stevec / d; imenovalec = r.imenovalec / d}

    let ( =%= ) (r : racionalno) (s : racionalno) : bool = r.stevec * s.imenovalec = s.stevec * r.imenovalec

    let ( <%< ) (r : racionalno) (s : racionalno) : bool = 
        let r' = kanonicno_racionalno r in
        let s' = kanonicno_racionalno s in
        r'.stevec * s'.imenovalec < s'.stevec * r'.imenovalec

    let ( >%> ) (r : racionalno) (s : racionalno) : bool = s <%< r

    let ( +%+ ) (r : racionalno) (s : racionalno) : racionalno = {stevec = r.stevec * s.imenovalec + s.stevec * r.imenovalec; imenovalec = r.imenovalec * s.imenovalec}  (* seštevanje racionalnih *)

    let minus_rat (r : racionalno) : racionalno = {stevec = -r.stevec; imenovalec = r.imenovalec}  (* nasprotna vrednost *)

    let ( -%- ) (r : racionalno) (s : racionalno) : racionalno = r +%+ minus_rat s  (* odštevanje racionalnih *)

    let ( *%* ) (r : racionalno) (s : racionalno) : racionalno = {stevec = r.stevec * s.stevec; imenovalec = r.imenovalec * s.imenovalec}

    let inverse_rat (r : racionalno) : racionalno = {stevec = r.imenovalec; imenovalec = r.stevec}  (* obratna vrednost *)

    let ( /%/ ) (r : racionalno) (s : racionalno) : racionalno = r *%* inverse_rat s

    let rat_je_int (r : racionalno) : bool = r.stevec mod r.imenovalec = 0

    let int_v_rat (n : int) : racionalno = {stevec = n; imenovalec = 1}

    let sign_rat (r : racionalno) : int = 
        if r.stevec = 0 then 0
        else
            if r <%< int_v_rat 0 then -1
            else 1

    let ( % ) (m : int) (n : int) =
        (* pythonski modulo *)
        if m * n < 0 then
            (m mod n) + n
        else
            m mod n
    (*
    5 % 3 = 2
    (-5) % 3 = 1
    (-5) % (-3) = -2
    5 % -3 = -1
    *)

    let floor_rat (r : racionalno) : int = (r.stevec - r.stevec % r.imenovalec) / r.imenovalec

    let zaokrozi_rat_proti_nicli (r : racionalno) : int = (r.stevec - r.stevec mod r.imenovalec) / r.imenovalec  (* manj uporabna, verjetno *)

    let ceil_rat (r : racionalno) : int = -floor_rat (minus_rat r)

    let celostevilsko_deljenje (r : racionalno) (s : racionalno) : int = floor_rat (r /%/ s)  (* vrne k v r = s * k + q, q*s >= 0 *)  (* ista funkcija kot pythonski // *)

    let modulo_rat (r : racionalno) (s : racionalno) : racionalno = r -%- (s *%* int_v_rat (celostevilsko_deljenje r s))  (* vrne q v r = s * k + q, q*s >= 0*)  (* pythonski % *)

    let rec pow (m : int) (n : int) : int =
        match n with
        | 0 -> 1
        | n' when n' > 0 -> m * pow m (n' - 1)
        | _ -> failwith "Negativno število"

    let rec pow_rat (r : racionalno) (n : int) : racionalno = 
        if n >= 0 then
            {stevec = pow r.stevec n; imenovalec = pow r.imenovalec n}
        else
            pow_rat (inverse_rat r) (-n)

    (*
    TODO
    let gcd_rat (r : racionalno) (s : racionalno) : racionalno = r

    let gcd_rat_extended (r : racionalno) (s : racionalno) : racionalno * int * int = (r, 1, 1)
    *)

    let izpisi_racionalno (r : racionalno) = 
        if rat_je_int r then
            Int.to_string (r.stevec / r.imenovalec)  (* celoštevilsko deljenje *)
        else
            let r' = kanonicno_racionalno r
            in
            Int.to_string r'.stevec ^ "/" ^ Int.to_string r'.imenovalec

end