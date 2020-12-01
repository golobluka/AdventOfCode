
let naloga1 vsebina_datoteke =
    let rec vsebuje stevilo = function
        | [] -> false
        | glava :: rep -> if glava = stevilo then true
            else vsebuje stevilo rep
    in
    let rec skrcitelj = function
    | [] -> -1
    | glava :: rep -> pregledovalec glava rep
    and pregledovalec fiksno_stevilo seznam_nadalnih = 
        if seznam_nadalnih |> List.map (fun x -> x + fiksno_stevilo) |> (vsebuje 2020) then fiksno_stevilo * (2020 - fiksno_stevilo)
        else skrcitelj seznam_nadalnih
    in
    string_of_int (skrcitelj vsebina_datoteke)

let naloga2 vsebina_datoteke =
     let rec vsebuje stevilo = function
        | [] -> false
        | glava :: rep -> if glava = stevilo then true
            else vsebuje stevilo rep
    in
    let rec skrcitelj fiksirano_st1  sez = 
        match fiksirano_st1, sez with
    | _ , [] -> -1
    | Some m , (glava :: rep as seznam) -> pregledovalec m glava rep seznam
    | None, (glava :: rep) -> skrcitelj  (Some glava) rep
    
    and pregledovalec fiksirano_st1 fiksirano_st2 zacasni_seznam seznam = 
        match zacasni_seznam with
        | [] -> skrcitelj None seznam
        | glava :: rep -> (
            if zacasni_seznam |> List.map (fun x -> x + fiksirano_st2 + fiksirano_st1) |> (vsebuje 2020) 
                then fiksirano_st2 * fiksirano_st1 * (2020 - fiksirano_st2 - fiksirano_st1)
            else 
            pregledovalec fiksirano_st1 glava rep seznam  
            )
    in
    string_of_int (skrcitelj None vsebina_datoteke)

let zapisi () =
(*    let preberi_datoteko ime_datoteke =
        let chan = open_in ime_datoteke in
        let vsebina = really_input_string chan (in_channel_length chan) in
        close_in chan;
        vsebina *)
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
    let vsebina_datoteke = List.map (fun x -> int_of_string x) (read_file "day_1.in") in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_1_1.out" odgovor1;
    izpisi_datoteko "day_1_2.out" odgovor2