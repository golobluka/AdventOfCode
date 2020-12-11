


let naloga1 sez_sedezev =
    (*V mainloopu se vedno znova nadgrajuje stanje, ki je izračunano s tremi gnezdenimi funkcijami. To so: 1) novo_stanje ki je repna rekurzija po vrsticah zapisa stanja ki nato pokliče funkcijo nova_vrstica. 2) nova_vrstica je repna rekurzija po karakterjih v stringu vrstice, ta nadalno pokliče 3) prestej_sosede, ki deluje nekako kot rekurzija po stevilih 1 - 3, 5 -8 ki kodirajo vseh osem različnih strani, na katerih najdemo sosede.*)

    (*  
        0  1  2
        3     5
        6  7  8
    *)
    let prestej_sosede n x y z = 
        let rec aux count st_sosedov =
            if count >= 9 then st_sosedov 
            else if count = 4 then aux (count+1) st_sosedov
            else if  count / 3 = 0 then 
                if String.get x ((n-1) + (count mod 3)) = '#' then aux (count+1) (st_sosedov+1) else aux (count+1) (st_sosedov)
            else if count / 3 = 1 then
                if String.get y ((n-1) + (count mod 3)) = '#' then aux (count+1) (st_sosedov+1) else aux (count+1) (st_sosedov)
            else (*if count / 3 = 2 then *)
                if String.get z ((n-1) + (count mod 3)) = '#' then aux (count+1) (st_sosedov+1) else aux (count+1) (st_sosedov)
        in
    aux 0 0
    in
        
    let rec nova_vrstica x y z =
        let rec aux count acc x y z = 
            if count < (String.length y) - 1 then
                let  opazovano = String.get y count in
                match opazovano with
                | 'L' -> if prestej_sosede count x y z = 0 then aux (count+1) (acc ^ "#") x y z 
                    else aux (count+1) (acc ^ "L") x y z
                | '#' -> if prestej_sosede count x y z >= 4 then aux (count+1) (acc ^ "L") x y z 
                    else aux (count+1) (acc ^ "#") x y z
                | '.' -> aux (count+1) (acc ^ ".") x y z
                | _ -> failwith "nepricakovan karakter v funkciji nova vrstica"
            else acc ^ "."
        in
        aux 1 "." x y z
    in

    let rec novo_stanje acc = function
        | x' :: y' :: [] -> 
            let zacetni = String.make (String.length x') '.' in
             [zacetni] @ List.rev acc @ [zacetni]
        | x :: (y :: z :: xs as t) -> novo_stanje (nova_vrstica x y z :: acc) t
        | _ -> failwith "Napaka pri funkciji novo_stanje"
    in
    
    let rec prestej count vrstice =
        (*Presteje koliko ljudi sedi na sedezih.*)
        let rec aux count1 count2  vrstica = 
            if count2 >= String.length vrstica then count1
            else if String.get vrstica count2 = '#'
                then aux (count1+1) (count2+1) vrstica
                else aux count1 (count2+1) vrstica
        in

        match vrstice with
        | [] -> count
        | head :: tail -> prestej (count + aux 0 0 head)  tail
    in
    let rec  enaki_stanji st_1 st_2 = 
        match st_1, st_2 with
        | [] , [] -> true
        | [] , _ | _ , [] -> false
        | x :: xs , y :: ys -> if String.equal x y then enaki_stanji xs ys
            else  false
    in

    let rec main_loop stanje = 
        let novo = novo_stanje [] stanje in
        if enaki_stanji novo stanje then prestej 0 stanje 
        else main_loop novo
    in
    
    main_loop sez_sedezev


let naloga2 sez_sedezev =
    
    let rec prestej count vrstice =
    (*Presteje koliko ljudi sedi na sedezih*)
        let rec aux count1 count2  vrstica = 
            if count2 >= String.length vrstica then count1
            else if String.get vrstica count2 = '#'
                then aux (count1+1) (count2+1) vrstica
                else aux count1 (count2+1) vrstica
        in

        match vrstice with
        | [] -> count
        | head :: tail -> prestej (count + aux 0 0 head)  tail
    in
    let rec  enaki_stanji st_1 st_2 = 
        match st_1, st_2 with
        | [] , [] -> true
        | [] , _ | _ , [] -> false
        | x :: xs , y :: ys -> if String.equal x y then enaki_stanji xs ys
            else  false
    in

    let rec main_loop trenutno_stanje =

        let preglej_diagonale vrsticni_stevec stolpicni_stevec vrstica =
            let poglej x y =
                let rec aux2 count =
                    if  vrsticni_stevec + count * x < 0 || vrsticni_stevec + count * x >= (String.length vrstica) then false
                    else if stolpicni_stevec + count * y < 0 || stolpicni_stevec + count * y >= (List.length trenutno_stanje) then false
                    else if String.get (List.nth trenutno_stanje (stolpicni_stevec + count * y)) (vrsticni_stevec + count * x) = 'L' then false
                    else if String.get (List.nth trenutno_stanje (stolpicni_stevec + count * y)) (vrsticni_stevec + count * x) = '#' then true
                    else aux2 (count+1)
                in
                aux2 1
            in
            let rec aux1 count st_sosedov =
                if count >= 9 then st_sosedov
                else if count = 4 then aux1 (count+1) st_sosedov
                else if poglej (count/3 - 1) ((count mod 3) - 1) then aux1 (count+1) (st_sosedov + 1)
                    else aux1 (count+1) st_sosedov
            in
            aux1 0 0
        in

        let rec nova_vrstica stolpicni_stevec x =
            let rec aux count acc x = 
                if count < (String.length x) - 1 then
                    let  opazovano = String.get x count in
                    match opazovano with
                    | 'L' -> if preglej_diagonale count stolpicni_stevec x = 0 then aux (count+1) (acc ^ "#") x 
                        else aux (count+1) (acc ^ "L") x
                    | '#' -> if preglej_diagonale count stolpicni_stevec x >= 5 then aux (count+1) (acc ^ "L") x 
                        else aux (count+1) (acc ^ "#") x
                    | '.' -> aux (count+1) (acc ^ ".") x
                    | _ -> failwith "nepricakovan karakter v funkciji nova vrstica"
                else acc ^ "."
            in
            aux 1 "." x
        in

        let rec novo_stanje acc stolpicni_stevec = function
            | [x'] -> 
                let zacetni = String.make (String.length x') '.' in
                 [zacetni] @ List.rev acc @ [zacetni]
            | x :: (y :: xs as t) -> novo_stanje (nova_vrstica stolpicni_stevec y :: acc) (stolpicni_stevec+1)  t
            | _ -> failwith "Napaka pri funkciji novo_stanje"
        in
        
        
        let novo = novo_stanje [] 1 trenutno_stanje in
        (*Main_loop teče vse dokler staje ne preslika nazaj v isto stanje. Torej dokler ne dosežemo fiksne točke.*)
        if enaki_stanji novo trenutno_stanje 
            then prestej 0 trenutno_stanje   
        else 
            main_loop novo
    in
    
    main_loop sez_sedezev



    

    


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
    let vsebina_datoteke = read_file "day_11.in" in
    let odgovor1 = string_of_int (naloga1 vsebina_datoteke)
    and odgovor2 = string_of_int (naloga2 vsebina_datoteke)
    in
    izpisi_datoteko "day_11_1.out" odgovor1;
    izpisi_datoteko "day_11_2.out" odgovor2
