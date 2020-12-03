let rec naloga1  sez_dreves =
    let rec aux acc pozicija stevec sez_dreves (zamik_desno, zamik_navzdol)  = 
        match sez_dreves with
        | [] -> acc
        | head :: tail -> if zamik_navzdol <> 1 && (stevec mod zamik_navzdol) = 0 then aux acc pozicija (stevec + 1) tail (zamik_desno, zamik_navzdol)
            else if String.get head (pozicija mod 31) = '#' then aux (acc + 1) (pozicija + zamik_desno) (stevec+1)  tail (zamik_desno, zamik_navzdol)
            else aux acc (pozicija + zamik_desno) (stevec+1) tail (zamik_desno, zamik_navzdol)
    in
    aux 0 0 1 sez_dreves (3,1)

let rec naloga2  sez_dreves =
    let rec aux acc pozicija stevec sez_dreves (zamik_desno, zamik_navzdol)  = 
        match sez_dreves with
        | [] -> acc
        | head :: tail -> if zamik_navzdol <> 1 && (stevec mod zamik_navzdol) = 0 then aux acc pozicija (stevec + 1) tail (zamik_desno, zamik_navzdol)
            else if String.get head (pozicija mod 31) = '#' then aux (acc + 1) (pozicija + zamik_desno) (stevec+1)  tail (zamik_desno, zamik_navzdol)
            else aux acc (pozicija + zamik_desno) (stevec+1) tail (zamik_desno, zamik_navzdol)
    in
    List.fold_left (fun x y -> x * y) 1 (List.map (aux 0 0 1 sez_dreves) [(1,1);(3,1);(5,1);(7,1);(1,2)])


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
    let vsebina_datoteke = read_file "day_3.in" in
    let odgovor1 = string_of_int (naloga1 vsebina_datoteke)
    and odgovor2 = string_of_int (naloga2 vsebina_datoteke)
    in
    izpisi_datoteko "day_3_1.out" odgovor1;
    izpisi_datoteko "day_3_2.out" odgovor2

