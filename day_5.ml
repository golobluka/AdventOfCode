let rec razdvoji mesto sez  =
    let rec aux acc1 acc2 k =
        if k <= 0 then (List.rev acc1, acc2)
        else match acc2 with
        | [] -> failwith "Prekratek seznam!"
        | head :: tail -> aux (head :: acc1) tail (k-1)
    in
    aux [] sez mesto

let rec int_power x = function
    | 0 -> 1
    | n -> x * int_power x (n-1)

let rec max acc = function
        | [] -> acc
        | glava :: rep -> if glava < acc then max acc rep 
            else max glava rep
    


let naloga1 sez_kod = 
    let sestej sez = List.fold_left (+) 0 sez 
    in
    let v_stevila = function
        | 'F' | 'L' -> 0
        | 'B' | 'R' -> 1
        | _ -> failwith "Nepricakovan karakter v nizu!"
    in 
    let rec pretvori_obliko_in_obrni acc s = 
        (* Vzame prazen akumulator ter string kode ter vrne seznam enic in ničel (v smislu dvojiskega zapisa)*) 
        let l = List.length acc in
        if l >= String.length s then acc 
        else
            pretvori_obliko_in_obrni ((v_stevila (String.get s l)) :: acc) s 
    in
    let rec aux acc sez_kod =
     (*To je glaven rekurziven klic, ki string v seznamu pretvori v ID vrednost*) 
        match sez_kod with
        | [] -> acc
        | glava :: rep -> 
            let koda_v_vrsti, koda_vrsta = razdvoji 3 (pretvori_obliko_in_obrni [] (glava)) in
            let rezultat =  (sestej (List.mapi (fun i x -> x * (int_power 2 i))  koda_vrsta)) * 8  + sestej(List.mapi (fun i x -> x * (int_power 2 i))  koda_v_vrsti)  in
                aux (rezultat :: acc) rep 
    in

    max (-1) (aux [] sez_kod)

let naloga2 sez_kod =
    let sestej sez = List.fold_left (+) 0 sez 
    in
    let v_stevila = function
        | 'F' | 'L' -> 0
        | 'B' | 'R' -> 1
        | _ -> failwith "Nepricakovan karakter v nizu!"
    in 
    let rec pretvori_obliko_in_obrni acc s =
        (* Vzame prazen akumulator ter string kode ter vrne seznam enic in ničel (v smislu dvojiskega zapisa)*) 
        let l = List.length acc in
        if l >= String.length s then acc 
        else
            pretvori_obliko_in_obrni ((v_stevila (String.get s l)) :: acc) s 
    in
    let rec aux acc sez_kod = 
        (*To je glaven rekurziven klic, ki string v seznamu pretvori v ID vrednost*) 
        match sez_kod with
        | [] -> acc
        | glava :: rep -> 
            let koda_v_vrsti, koda_vrsta = razdvoji 3 (pretvori_obliko_in_obrni [] (glava)) in
            let rezultat =  (sestej (List.mapi (fun i x -> x * (int_power 2 i))  koda_vrsta)) * 8  + sestej(List.mapi (fun i x -> x * (int_power 2 i))  koda_v_vrsti)  in
                aux (rezultat :: acc) rep 
    in
    let rec najdi_niso = function
    | [] | [_] -> failwith "ni nise"
    | glava :: (vrat :: _ as tail) -> if glava+1 >= vrat then najdi_niso tail 
        else  glava+1
    in
    let urejen_seznam_ID = List.sort compare (aux [] sez_kod) in
        najdi_niso urejen_seznam_ID

   
        


let zapisi () =
    (* let preberi_datoteko ime_datoteke =
        let chan = open_in ime_datoteke in
        let vsebina = really_input_string chan (in_channel_length chan) in
        close_in chan;
        vsebina
    in *)
    (*funkcija read_file preuzeta iz:
    https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml*)
    let read_file filename = 
        let lines = ref [] in
        let chan = open_in filename in
        try
          while true; do
            lines := input_line chan :: !lines
          done; !lines
        with End_of_file ->
          close_in chan;
          List.rev !lines 
    and izpisi_datoteko ime_datoteke vsebina =
        let chan = open_out ime_datoteke in
        output_string chan vsebina;
        close_out chan
    in
    let vsebina_datoteke = read_file "day_5.in" in
    let odgovor1 = string_of_int (naloga1 vsebina_datoteke)
    and odgovor2 = string_of_int (naloga2 vsebina_datoteke)
    in
    izpisi_datoteko "day_5_1.out" odgovor1;
    izpisi_datoteko "day_5_2.out" odgovor2
