let rec print_line (v : Satsolver.valuation) : unit =
  match v with
  | [] -> ()
  | (x, true) :: q -> print_string ("| 1 "); print_line q
  | (x, false) :: q -> print_string ("| 0 "); print_line q

let print_tab (f : Satsolver.formule) : unit =
  let rec aux (v : Satsolver.valuation option) : unit =
    match v with
    | Some u -> print_line u; 
        let res = Satsolver.interpret_valuation u f in 
        print_string ("| ");
        print_string (if res then "1" else "0");
        print_string ("\n"); 
        aux (Satsolver.valuation_next u)
      
    | None -> print_newline ()
  in
  aux (Some (Satsolver.valuation_init (Satsolver.liste_var f)))

let rec print_vars l = match l with 
  | [] -> print_string "  phi\n\n"
  | x :: q -> print_string "  "; print_string (x); print_string " "; print_vars q

let main () = 
  let f = Satsolver.parse Sys.argv.(1) in
  print_vars (Satsolver.liste_var f);
  print_tab f

let _ = main ()