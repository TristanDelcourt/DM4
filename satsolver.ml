open Hashtbl
open Printf

type formule =
  | Var of string
  | Id of int
  | Top
  | Bot
  | And of formule * formule
  | Or of formule * formule
  | Not of formule

let implique (f1, f2) = Or (Not f1, f2)
let equivalence (f1, f2) = And (implique (f1, f2), implique (f2, f1))

(*** PARSER ***)

exception Erreur_syntaxe
exception Fichier_invalide

(* Symboles:
   'T' -> true
   'F' -> false
   '&' -> And
   '|' -> Or
   '~' -> Not
   '>' -> implication
   '=' -> equivalence
*)

(* Détermine si c correspond à un opérateur binaire logique *)
let is_binop (c : char) : bool =
  match c with '&' | '|' | '>' | '=' -> true | _ -> false

(* Priorité de l'opérateur c. Permet de déterminer
   comment interpréter une formule sans parenthèses.
   Par exemple, "x&y|z" sera interprété comme "(x&y)|z"
   car & est plus prioritaire que | *)
let priority (c : char) : int =
  match c with
  | '&' -> 4
  | '|' -> 3
  | '=' -> 2
  | '>' -> 1
  | _ -> raise Erreur_syntaxe (* c n'est pas un opérateur *)

(* indice de l'opérateur le moins prioritaire parmis ceux
   qui ne sont pas entre parenthèses entre s.[i] et s.[j]
   inclus *)
let find_op_surface (s : string) (i : int) (j : int) : int =
  (*
      Renvoie l'indice de l'opérateur le moins prioritaire entre
      i et j, sachant que res est l'indice du meilleur opérateur
      entre i et k-1.
      paren_lvl: niveau d'imbrication actuel des parenthèses *)
  let rec find_op_paren (k : int) (res : int) (paren_lvl : int) : int =
    if k = j + 1 then res
    else if s.[k] = '(' then find_op_paren (k + 1) res (paren_lvl + 1)
    else if s.[k] = ')' then find_op_paren (k + 1) res (paren_lvl - 1)
      (* Le caractère lu est pris si l'on est hors des parenthèses,
         que le caractère est bien un opérateur, et qu'il est moins
         prioritaire que le meilleur résultat jusqu'ici *)
    else if
      paren_lvl = 0
      && is_binop s.[k]
      && (res = -1 || priority s.[k] < priority s.[res])
    then find_op_paren (k + 1) k paren_lvl
    else find_op_paren (k + 1) res paren_lvl
  in
  find_op_paren i (-1) 0

(* Renvoie une formule construite à partir de la chaîne s.
   Lève une exception Erreur_syntaxe si la chaîne ne représente pas une formule valide. *)
let parse (s : string) : formule =
  let n = String.length s in
  (* construit une formule à partir de s[i..j] *)
  let rec parse_aux (i : int) (j : int) =
    if not (0 <= i && i < n && 0 <= j && j < n && i <= j) then
      raise Erreur_syntaxe
    else if s.[i] = ' ' then parse_aux (i + 1) j
    else if s.[j] = ' ' then parse_aux i (j - 1)
    else
      let k = find_op_surface s i j in
      if k = -1 then
        if s.[i] = '~' then Not (parse_aux (i + 1) j)
        else if s.[i] = '(' then
          if s.[j] != ')' then
            (*
            print_int j;*)
            failwith "mauvais parenthésage"
          else parse_aux (i + 1) (j - 1)
        else if i = j && s.[i] = 'T' then Top
        else if i = j && s.[i] = 'F' then Bot
        else Var (String.sub s i (j - i + 1))
      else
        match s.[k] with
        | '&' -> And (parse_aux i (k - 1), parse_aux (k + 1) j)
        | '|' -> Or (parse_aux i (k - 1), parse_aux (k + 1) j)
        | '=' -> equivalence (parse_aux i (k - 1), parse_aux (k + 1) j)
        | '>' -> implique (parse_aux i (k - 1), parse_aux (k + 1) j)
        | _ -> raise Erreur_syntaxe
  in
  parse_aux 0 (String.length s - 1)

(* Renvoie une formule construire à partir du contenu du fichier fn.
   Lève une exception Erreur_syntaxe si le contenu du fichier n'est pas une formule valide.
   Lève une exception Sys_error(message_erreur) si le nom du fichier n'est pas valide. *)
let from_file (filename : string) : formule =
  (* concatène toutes les lignes de f en une seule chaîne *)
  let rec read_lines f =
    try
      let next_line = input_line f in
      let s = read_lines f in
      next_line ^ s
    with End_of_file -> ""
  in
  let f = open_in filename in
  let s = read_lines f in
  parse s

(* Renvoie le contenu du fichier fn sous forme de string.
   Le fichier ne doit contenir qu'une seule ligne *)
let read_file (fn : string) : string =
  let ic = open_in fn in
  let res = input_line ic in
  close_in ic;
  res

(*** Manipulation de formules logiques ***)

(*compte ops f renvoie le nombre d'opérateurs utilisés dans f*)
let rec compte_ops (f : formule) : int =
  match f with
  | Var _ | Top | Bot | Id _ -> 0
  | And (a, b) | Or (a, b) -> 1 + compte_ops a + compte_ops b
  | Not a -> 1 + compte_ops a
 
(*verifie si une liste est triee dans l'ordre croissant et sans doublon*)
let rec st_croissant (l : 'a list) : bool =
  match l with
  | [] | [ _ ] -> true
  | x :: y :: q -> if x < y then st_croissant (y :: q) else false

(* pour deux ensembles l1 et l2 triés, renvoie leur union trié*)
let rec union (l1 : 'a list) (l2 : 'a list) : 'a list =
  assert (st_croissant l1);
  assert (st_croissant l2);
  match l2 with
  | [] -> l1
  | x :: q -> (
      match l1 with
      | [] -> l2
      | y :: p ->
          if x = y then x :: union p q
          else if x < y then x :: union q l1
          else y :: union l2 p)

(* renvoie les variables d'une formule f (sans doublon) *)
let liste_var (f : formule) : string list =
  let rec aux f2 l =
    match f2 with
    | Var x -> union l [ x ]
    | And (a, b) | Or (a, b) -> union (aux a l) (aux b l)
    | Not a -> aux a l
    | Top | Bot -> l
    | Id n -> failwith "liste_var appelé sur un formule composé de ID"
  in
  aux f []

(*** Valuations et SAT-solver naif ***)
type valuation = (string * bool) list
type sat_result = valuation option

(* revoir true ou false correspondant a la valeur trouver en interpretant une formule avec une certaine valuation*)
let rec interpret_valuation (v : valuation) (f : formule) : bool =
  match f with
  | Var x -> List.assoc x v
  | Top -> true
  | Bot -> false
  | And (a, b) -> interpret_valuation v a && interpret_valuation v b
  | Or (a, b) -> interpret_valuation v a || interpret_valuation v b
  | Not a -> not (interpret_valuation v a)
  | Id n -> failwith "Formule avec des ID, impossible de savoir son string associé"

(* pour un nombre binaire écrit dans une liste renvoie le même nombre binaire + 1*)
let rec add_one (l : bool list) : bool list =
  match l with
  | [] -> [ true ]
  | false :: q -> true :: q
  | true :: q -> false :: add_one q

(*Renvoie la "prochaine" valuation, None si c'est la dernière*)
let valuation_next (v : valuation) : valuation option =
  let bools = List.map (fun (x, y) -> y) v in
  let vars = List.map (fun (x, y) -> x) v in
  let bools_next = add_one bools in
  if List.length bools <> List.length bools_next then None
  else Some (List.map2 (fun x y -> (x, y)) vars bools_next)

(*renvoie la première valuation*)
let valuation_init (variables : string list) : valuation =
  List.map (fun x -> (x, false)) variables

(*résout SAT pour f*)
let satsolver_naif (f : formule) : sat_result =
  let rec tester_valuation (v : valuation option) : sat_result =
    match v with
    | Some u ->
        if interpret_valuation u f then v
        else tester_valuation (valuation_next u)
    | None -> None
  in
  tester_valuation (Some (valuation_init (liste_var f)))

(*** Algorithme de Quine ***)

(* essaie de simplifie une formule, renvoie un couple avec la formule simplié (ou non) et un booléen indiquant si elle est simplifié *)
let rec simpl_step (f : formule) : formule * bool =
  match f with
  | And (Top, phi) | And (phi, Top) -> (phi, true)
  | And (Bot, phi) | And (phi, Bot) -> (Bot, true)
  | Or (Bot, phi) | Or (phi, Bot) -> (phi, true)
  | Or (Top, phi) | Or (phi, Top) -> (Top, true)
  | Not (Not phi) -> (phi, true)
  | Not Top -> (Bot, true)
  | Not Bot -> (Top, true)
  | And (phi, psi) ->
      let f1, b1 = simpl_step phi in
      let f2, b2 = simpl_step psi in
      (And (f1, f2), b1 || b2)
  | Or (phi, psi) ->
      let f1, b1 = simpl_step phi in
      let f2, b2 = simpl_step psi in
      (Or (f1, f2), b1 || b2)
  | Not phi ->
      let f1, b1 = simpl_step phi in
      (Not f1, b1)
  | _ -> (f, false)

(*simplifie entièrement une formule f*)
let rec simpl_full (f : formule) : formule =
  let f1, b = simpl_step f in
  if b then simpl_full f1 else f

(*renvoie f dans laquelle on a remplacé x par g*)
let rec subst (f : formule) (x : string) (g : formule) : formule =
  match f with
  | Var y -> if y = x then g else f
  | And (a, b) -> And (subst a x g, subst b x g)
  | Or (a, b) -> Or (subst a x g, subst b x g)
  | Not a -> Not (subst a x g)
  | _ -> f

let quine (f : formule) : sat_result =
  let rec aux (f1 : formule) (vars : string list) : sat_result =
    match f1 with
    | Top -> Some (valuation_init vars)
    | Bot -> None
    | _ -> (match vars with 
      | x::q -> (
        (*Printf.printf "%s\n%!" x;*)
        match aux (simpl_full (subst f1 x Top)) q with
        | Some v -> Some ((x, true) :: v)
        | None -> (
            match aux (simpl_full (subst f1 x Bot)) q with
            | Some v -> Some ((x, false) :: v)
            | None -> None))
      | _ -> failwith "impossible")
  in
  aux (simpl_full f) (liste_var f)

let print_true (s : sat_result) =
  match s with
  | None -> print_string "Pas de solution\n"
  | Some v1 ->
      print_string
        "La formule est satisfiable en assignant 1 aux variables suivantes et \
         0 aux autres\n";
      let rec aux (v : valuation) : unit =
        match v with
        | [] -> ()
        | (x, true) :: q ->
            print_string x;
            print_newline ();
            aux q
        | (x, false) :: q -> aux q
      in
      aux v1

(*quine v2*)
let rec simpl_step_compt (f : formule) : formule * int =
  match f with
  | And (Top, phi) | And (phi, Top) -> (phi, 1)
  | And (Bot, phi) | And (phi, Bot) -> (Bot, 1)
  | Or (Bot, phi) | Or (phi, Bot) -> (phi, 1)
  | Or (Top, phi) | Or (phi, Top) -> (Top, 1)
  | Not (Not phi) -> (phi, 1)
  | Not Top -> (Bot, 1)
  | Not Bot -> (Top, 1)
  | And (phi, psi) ->
      let f1, b1 = simpl_step_compt phi in
      let f2, b2 = simpl_step_compt psi in
      (And (f1, f2), b1 + b2)
  | Or (phi, psi) ->
      let f1, b1 = simpl_step_compt phi in
      let f2, b2 = simpl_step_compt psi in
      (Or (f1, f2), b1 + b2)
  | Not phi ->
      let f1, b1 = simpl_step_compt phi in
      (Not f1, b1)
  | _ -> (f, 0)

let rec simpl_full_compt (f : formule) : formule * int =
  let f1, b1 = simpl_step_compt f in
  if b1 > 0 then
    let f2, b2 = simpl_full_compt f1 in
    (f2, b1 + b2)
  else (f, b1)

let quine_v2 (f : formule) : sat_result =
  let rec aux (f1 : formule) (vars : string list) : sat_result =
    match f1 with
    | Top -> Some (valuation_init vars)
    | Bot -> None
    | _ -> (match vars with
      | x :: q -> (
        (*Printf.printf "%s\n%!" x;*)
        let f2, b1 = simpl_full_compt (subst f1 x Top) in
        let f3, b2 = simpl_full_compt (subst f1 x Bot) in
        if b1 > b2 then
          (match aux f2 q with
            | Some v -> Some ((x, true) :: v)
            | None -> (
                match aux f3 q with
                | Some v -> Some ((x, false) :: v)
                | None -> None))
        else 
          (match aux f3 q with
            | Some v -> Some ((x, false) :: v)
            | None -> (
                match aux f2 q with
                | Some v -> Some ((x, true) :: v)
                | None -> None)))
      | _ -> failwith "impossible")
  in
  aux (simpl_full f) (liste_var f)
(*end quine v2*)

(*quine FNC*)

(*soit f une formule, renvoie un booléen selon si la formule est sous FNC ou non*)
let est_fnc (f : formule) : bool = 
  let rec aux (b_ou : bool) (b_non : bool) (f1 : formule) = 
    if (b_non) then (match f1 with 
      | Var _ | Top | Bot -> true
      | _ -> false )
    else 
      (if (b_ou) then (match f1 with
        | Or (f2,f3) -> (aux b_ou b_non f2)&&(aux b_ou b_non f3)
        | Var _ | Top | Bot -> true
        | Not f2 -> aux b_ou true f2 
        | _ -> false)
      else (match f1 with 
        | Var _ | Top | Bot -> true
        | Not f2 -> aux b_ou true f2
        | Or (f2,f3) -> (aux true b_non f2)&&(aux true b_non f3)
        | And (f2,f3) -> (aux b_ou b_non f2)&&(aux b_ou b_non f3)
        | Id n -> failwith "pas le bon algo"))
  in aux false false f

let rec creer_val (s : sat_result) (v : valuation) : sat_result =
  match s with
  | None -> None
  | Some v1 ->
      let rec aux (v2 : valuation) (v3 : valuation) : valuation =
        match v3 with
        | [] -> []
        | (x, b) :: q -> (
            match List.find_opt (fun (x1, b1) -> x1 = x) v2 with
            | None -> (x, false) :: aux v2 q
            | Some (x1, b1) -> (x1, b1) :: aux v2 q)
      in
      Some (aux v1 v)

let rec find_first_variable (f: formule): string = match f with
  | Var x | Not (Var x) -> x
  | Or (a,b) | And (a, b) -> find_first_variable a
  | _ -> "" (* la formule est composé seulement de Top et Bot, le resultat ne sera pas traité *)

let rec simplify_fnc (f : formule) (x : formule) : formule =
  let rec iterate f x =
    match (f, x) with
    | Var y, Var z when y = z -> Top
    | Var y, Not (Var z) when y = z -> Bot
    | Var y, _-> Var y
    
    | Not (Var y), Not (Var z) when y = z -> Top
    | Not (Var y), Var z when y = z -> Bot
    | Not (Var y), _ -> Not (Var y)
    
    
    | Or (Var y, b), Var z when y = z -> Top
    | Or (Not (Var y), b), Not (Var z) when y = z -> Top
    | Or (Var y, b), Not (Var z) when y = z -> iterate b x
    | Or (Not (Var y), b), Var z when y = z -> iterate b x
    
    
    | Or (Var y, b), _ | Or (b, Var y), _ -> (
      (* y une variable autre que x *)
      let f1 = iterate b x in
      match f1 with 
        | Top -> Top 
        | Bot -> Var y
        | _ -> Or (Var y, f1))
    | Or (Or(a1, b1), Or(a2,b2)) , _ -> (
      (* 2 OU *)
      let f1 = iterate (Or(a1, b1)) x in
      match f1 with 
        | Top -> Top 
        | _ -> let f2 = iterate (Or(a2, b2)) x in match f2 with
          | Top -> Top
          | Bot when f1 = Bot -> Bot
          | Bot when f1 != Bot -> f1
          | _ -> Or(f1, f2))
    | _ -> f
  in

  match f with
  | And (And(a1, b1), And (a2, b2)) -> ( (*ET avec 2 ET*)
    let f1 = simplify_fnc (And(a1, b1)) x in
    match f1 with
      | Bot -> Bot
      | _ -> 
        let f2 = simplify_fnc (And(a2, b2)) x in
        match f2 with
          | Bot -> Bot
          | Top when f1 = Top -> Top
          | Top when f1 != Top -> f1
          | _  when f1 = Top -> f2
          | _ -> And(f1, f2))
    
  | And (a, And (a1, b1)) | And (And (a1, b1), a)-> ( (*ET avec un ET et un OU*)
      let f1 = iterate a x in
      match f1 with
      | Bot -> Bot
      | _ -> 
        let f2 = simplify_fnc (And(a1, b1)) x in
        match f2 with
          | Bot -> Bot
          | Top when f1 = Top -> Top
          | Top when f1 != Top -> f1
          | _  when f1 = Top -> f2
          | _ -> And(f1, f2))
      
  | And (a, b) -> ( (* ET avec 2 OU*)
      let f1 = iterate a x in
      match f1 with
      | Bot -> Bot
      | _ -> 
        let f2 = iterate b x in
        match f2 with
          | Bot -> Bot
          | Top when f1 = Top -> Top
          | Top when f1 != Top -> f1
          | _  when f1 = Top -> f2
          | _ -> And(f1, f2))
  | _ -> iterate f x (* Plus qu'une conjonction *)

let quine_fnc (f : formule) : sat_result =
  let rec aux (f1 : formule) (x: string) : sat_result =
    match f1 with
    | Top -> Some (valuation_init (liste_var f1))
    | Bot -> None
    | _ -> 
        let f2 = simplify_fnc f1 (Var x) in
        match aux f2 (find_first_variable f2) with
          | Some v -> Some ((x, true) :: v)
          | None -> (
              let f3 = simplify_fnc f1 (Not (Var x)) in
              match aux f3 (find_first_variable f3) with
              | Some v -> Some ((x, false) :: v)
              | None -> None)
  in

  let vars = liste_var f in
  let f = simpl_full f in
  creer_val (aux f (find_first_variable f)) (valuation_init vars)
(*end quine FNC*)

(*quine v3*)
type sat_result_id = (int*bool) list option


let create_hash (l: string list) = 
  let tovar = Hashtbl.create 300 in
  let toid = Hashtbl.create 300 in
  
  let rec fill_hash l1 n : int = match l1 with
    | [] -> n
    | x::q -> Hashtbl.add tovar n x;
              Hashtbl.add toid x n;
              fill_hash q (n+1)
  in
  let n = fill_hash l 0 in

  (n, tovar, toid)

let rec formule_var_to_id (f: formule) toid = 
  match f with
    | Var y -> Id (Hashtbl.find toid y)
    | And (a, b) -> And (formule_var_to_id a toid, formule_var_to_id b toid)
    | Or (a, b) -> Or (formule_var_to_id a toid, formule_var_to_id b toid)
    | Not a -> Not (formule_var_to_id a toid)
    | _ -> f

let rec subst_id (f : formule) (n : int) (g : formule) : formule =
match f with
| Id m -> if n = m then g else f
| And (a, b) -> And (subst_id a n g, subst_id b n g)
| Or (a, b) -> Or (subst_id a n g, subst_id b n g)
| Not a -> Not (subst_id a n g)
| _ -> f

let rec valuation_init_id n = 
  if n>=0 then (n, false)::valuation_init_id (n-1) else []


let id_to_vals (l: sat_result_id) d : sat_result = 
  match l with
    | None -> None
    | Some l1 -> 
      let rec aux l2 d2 = 
        match l2 with
          | [] -> Some []
          | (x, b)::q -> let v = aux q d2 in 
            (match v with 
              | None -> None 
              | Some v1 -> Some ((Hashtbl.find d2 x, b)::v1))
      in
      aux l1 d
  
let quine_v3 (f : formule) : sat_result =
  let rec aux (f1 : formule) (n: int) : sat_result_id =
    match f1 with
    | Top -> Some (valuation_init_id n)
    | Bot -> None
    | _ -> (
      match aux (simpl_full (subst_id f1 n Top)) (n-1) with
        | Some v -> Some ((n, true) :: v)
        | None -> (
          match aux (simpl_full (subst_id f1 n Bot)) (n-1) with
          | Some v -> Some ((n, false) :: v)
          | None -> None))
  in
  let n, tovar, toid = create_hash (liste_var f) in
  let f1 = formule_var_to_id f toid in
  let res = aux (simpl_full f1) (n-1) in
  id_to_vals res tovar


(*end quine v3*)

(*quine v4*)

let quine_v4 (f : formule) : sat_result =
  let rec aux (f1 : formule) (vars : string list) : sat_result =
    match f1 with
    | Top -> Some (valuation_init vars)
    | Bot -> None
    | And (a, b) when est_fnc (And(a, b)) -> quine_fnc (And(a, b))
    | _ -> (match vars with 
      | x::q -> (
        (*Printf.printf "%s\n%!" x;*)
        match aux (simpl_full (subst f1 x Top)) q with
        | Some v -> Some ((x, true) :: v)
        | None -> (
            match aux (simpl_full (subst f1 x Bot)) q with
            | Some v -> Some ((x, false) :: v)
            | None -> None))
      | _ -> failwith "impossible")
  in
  aux (simpl_full f) (liste_var f)

(*end quine v4*)

(*** Tests ***)

(*Teste la fonction qui compte le noùbre d'operateurs dans une formule*)
let test_compte_ops () =
  assert (compte_ops (parse "x | (y & ~z)") = 3);
  assert (compte_ops (parse "(x | (x & ~z) | y)") = 4)

(*Teste l'import d'une formule dans un fichier*)
let test_from_file () =
  assert (
    from_file "tests/test1.txt" = implique (And (Var "x", Var "y"), Var "z"));

  assert (from_file "tests/test2.txt" = Or (Var "x", Var "y"));

  assert (
    from_file "tests/test3.txt"
    = equivalence (And (Var "x", Var "y"), Or (Var "z", Var "y")))

(*Teste le parsing d'une formule sous forme de string*)
let test_parse () =
  assert (parse "a | (b & ~c)" = Or (Var "a", And (Var "b", Not (Var "c"))));

  assert (
    (try Some (parse "((a | b)") with
    | Failure str when str = "mauvais parenthésage" -> None
    | _ -> Some Top)
    = None)

(*test de la fonction interpret va*)
let test_interpret_valuation () =
  assert (
    interpret_valuation
      [ ("a", false); ("b", true); ("c", false) ]
      (parse "a | (b & ~c)")
    = true);

  assert (
    interpret_valuation [ ("a", false); ("b", false) ] (parse "a | b ") = false);

  assert (
    interpret_valuation
      [
        ("a", false);
        ("b", true);
        ("c", false);
        ("d", false);
        ("e", false);
        ("f", false);
        ("g", false);
        ("h", false);
        ("i", false);
      ]
      (parse "a | b | c | d | e | f | g | h | i")
    = true)

(*test de la fonction liste_var*)
let test_liste_var () =
  assert (liste_var (parse "x | (y & ~z) | y & x") = [ "x"; "y"; "z" ]);
  assert (liste_var (parse "a | ~b") = [ "a"; "b" ])

(*test de la fonction satsolver_naif*)
let test_SATnaif () =
  assert (satsolver_naif (parse "x & ~x") = None);
  assert (satsolver_naif (parse "x | ~x") != None);
  assert (
    satsolver_naif (parse "x & y & z ")
    = Some [ ("x", true); ("y", true); ("z", true) ])

(*test de la fonction simpl_full*)
let test_simpl_full () =
  assert (simpl_full (parse "T & a") = Var "a");
  assert (simpl_full (parse "T | a") = Top);
  assert (simpl_full (parse "T & (F | T | x | y) & F") = Bot)

let test_subst () =
  assert (subst (parse "x | y & ~z") "y" Top = parse "x | T & ~z");
  assert (subst (parse "a & b") "b" Bot = parse "a & F")

let test_quine () =
  let q1 f b =
    let formule = parse f in
    match quine formule with
    | None -> b
    | Some v -> interpret_valuation v formule
  in

  let q2 f b =
    let formule = parse f in
    match quine_v2 formule with
    | None -> b
    | Some v -> interpret_valuation v formule
  in

  let q3 f b =
    let formule = parse f in
    match quine_v3 formule with
    | None -> b
    | Some v -> interpret_valuation v formule
  in

  let qFNC f b =
    let formule = parse f in
    match quine_fnc formule with
    | None -> b
    | Some v -> interpret_valuation v formule
  in

  assert (q1 "x&~x" true);
  assert (q1 "x|y" false);
  assert (q1 "x & y | y & ~x" false);
  assert (q1 "a|b|c|d|e|f|g|h" false);
  assert (
    q1
      "~u=~g=~o>~u>j>~m&j=f>~g|t=~h&c>h|p|j>F>F=~x|~e=p>g|v&d=~h>~j=~v>c&~f=h&~r=t|~r>~s|~h&~g|~e=~a|~p>~q|~d=b>~z&i>~p|r&~g&v&~u=l=~w=a&~u|~g>~n|v>x&l|r>b|~o&p&~w&v>g&i|x&~v&q"
      false);

  assert (q2 "x&~x" true);
  assert (q2 "x|y" false);
  assert (q2 "x & y | y & ~x" false);
  assert (q2 "a|b|c|d|e|f|g|h" false);
  assert (
    q2
      "~u=~g=~o>~u>j>~m&j=f>~g|t=~h&c>h|p|j>F>F=~x|~e=p>g|v&d=~h>~j=~v>c&~f=h&~r=t|~r>~s|~h&~g|~e=~a|~p>~q|~d=b>~z&i>~p|r&~g&v&~u=l=~w=a&~u|~g>~n|v>x&l|r>b|~o&p&~w&v>g&i|x&~v&q"
      false);

  assert (q3 "x&~x" true);
  assert (q3 "x|y" false);
  assert (q3 "x & y | y & ~x" false);
  assert (q3 "a|b|c|d|e|f|g|h" false);
  assert (
    q3
      "~u=~g=~o>~u>j>~m&j=f>~g|t=~h&c>h|p|j>F>F=~x|~e=p>g|v&d=~h>~j=~v>c&~f=h&~r=t|~r>~s|~h&~g|~e=~a|~p>~q|~d=b>~z&i>~p|r&~g&v&~u=l=~w=a&~u|~g>~n|v>x&l|r>b|~o&p&~w&v>g&i|x&~v&q"
      false);

  assert (qFNC "(a|~b|c)&(a|b)&(~b|d|c)&(~b|~d)" false);
  assert (qFNC "(a|b|c)&a" false);
  assert (qFNC "(a|~b)&(~b|d|~c)" false);
  assert (qFNC "(x|y)&(y)" false)

let test_est_fnc () =
  assert (est_fnc (parse "(a|b) & y"));
  assert (est_fnc (parse "(a|b) & (y|~z|x) & (a | b)"));
  assert (est_fnc (parse "a > b"));
  assert (est_fnc (parse "(a|b)&~c"))
  
(*lance tous les tests*)
let test () =
  test_parse ();
  test_from_file ();
  test_compte_ops ();
  test_interpret_valuation ();
  test_liste_var ();
  test_SATnaif ();
  test_simpl_full ();
  test_subst ();
  test_quine ();
  test_est_fnc ();

  print_string "Tous les tests ont reussi\n"

let write_file path s =
  let file = open_out path in
  match s with
  | None ->
      output_string file "Pas de solution\n";
      close_out file
  | Some v1 ->
      output_string file
        "La formule est satisfiable en assignant 1 aux variables suivantes et \
         0 aux autres\n";
      let rec aux (v : valuation) : unit =
        match v with
        | [] -> ()
        | (x, true) :: q ->
            output_string file x;
            output_string file "\n";
            aux q
        | (x, false) :: q -> aux q
      in
      aux v1;
      close_out file

let help () =
  print_string
    "ATTENTION: Si vous souhaitez compiler le code, vous devez avoir ocaml \
     5.1.1 ou plus d'installé\n";
  print_string "\n\n  --  Documentation d'utilisation du satsolver  -- \n \n\n";
  print_string
    "Le satsolver a un argument obligatoire qui est le chemin du fichier \
     contenant la formule a analyser, on peut alors appeler le satsolver comme \
     ceci:\n\n\
    \   >>> ./satsolver <chemin du fichier> [arguments facultatifs]\n\n";
  print_string
    "Par défaut, le satsolver va detecter si la formule est sous FNC et \
     utiliser le resolveur optimisé pour cela, sinon il va utiliser Quine_v1. \
     Mais on peut modifier son comportement avec les arguments facultatifs \
     suivants:\n\n";
  print_string "  -hide             (n'affiche pas le resultat)\n";
  print_string "  -tofile <chemin>  (enregistre le resultat dans un fichier)\n";
  print_string
    "  -nofnc            (désactive la detection de formule fnc, et donc de \
     l'algorithme associé)\n";
  print_string
    "  -naif -q1 -q2 -q3 (utilise le resolveur naif/quine version 1/quine \
     version 2 (*))\n\n";
  print_string "Tout autre argument sera ignoré\n\n";
  print_string
    "(*) Si plusieurs solveurs sont donnés alors un l'orde est défini par naif>q1>q2>q3\n"

let fred () =
  print_string "                                      /@@@&#%@@@@@@                           \n";
  print_string "                                     .@@@@&#%@@@@@@%.                         \n";
  print_string "             .#@%#        .(#%#,     .@@@@#    .@@@@*,     ,%@@@@@@%.         \n";
  print_string "             /@@@@,       #@@@@(     .@@@@#     @@@@(.   /@@%*. .*@@@@/       \n";
  print_string "             &@@@@&      ,@@@@@&     .@@@@#    /@@@@,.             .@@@       \n";
  print_string "            *@@@@@@%     @@@@@@@.    .&@@@@@@@@@@@#,               .@@@       \n";
  print_string "            (@@@/&@@,   #@@@(@@@/    .&@@@% ..,*.                 .@@@        \n";
  print_string "            &@@@..@@@,  @@@*.@@@@    .&@@@#                      &@@/         \n";
  print_string "           (@@@#  %@@@,@@@*  @@@@.   .&@@@#                    @@@#           \n";
  print_string "           %@@@/  ,@@@@@@&   &@@@/   .&@@@#                 (@@@*             \n";
  print_string "           @@@@.   *@@@@@    (@@@#   .%@@@#               @@@@..              \n";
  print_string "          ,@@@@     %@@@,    ,@@@@.                      @@@@@@@@@@@@@@@*     \n";
  print_string "          ..          *         .                                             \n";
  print_string "                            *                                                 \n";
  print_string "                        &@@@@/                                  *,            \n";
  print_string "                     .@@##&@%                                  %@@@(          \n";
  print_string "                    *@###@@(                                   #@%&@/         \n";
  print_string "                    @@%@@&                                     ,@&&@/         \n";
  print_string "                 *@&&@@@@@@@@@&,                                @@@@          \n";
  print_string "                @&#@@(@@       ./%@@@@%*                        @@&@@         \n";
  print_string "               @&#@@..@@                ./%@@@@@@@&#,,         @@#&@@         \n";
  print_string "               @@@%   @@                              @@@@@@/#&@@@@@@%        \n";
  print_string "              @@@.    &@    /@%                                       &@@*    \n";
  print_string "             @@,      #@,    %@/                                      &@(     \n";
  print_string "           ,@/        /@*    .@@                                      &@,     \n";
  print_string "          @&          ,@#     @@                                      &@,     \n";
  print_string "         &@(          .@&    ,@&                                      %@@/    \n";
  print_string "        .@*            @@    .#.                                      @@@@@   \n";
  print_string "        /@.            &@.                                          ,@@@%&@.  \n";
  print_string "         @%            &@*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&&&&%%%%##@@&@@%%@&  \n";
  print_string "         %@            #@(                                       /@&#%@&##&@# \n";
  print_string "         /@* ,@@#      ,@#   ,&,                               *@@%#%@@###%@@ \n";
  print_string "          @@@&.         @&    &@*                            .@@%##%@@%%@@#@@.\n";
  print_string "          @@.           @@     @@.                          &@&###%@@##%@@#&@(\n";
  print_string "        .@@@#           &@     &@.                       ,@@%####%@&##%@@%#&@(\n";
  print_string "       %@/ @@           #@,    @@.                     (@@%#####&@%##%@@%##%@(\n";
  print_string "      (@/  #@,          /@/     ,                   ,@@&%######@@%###@@%###%@(\n";
  print_string "     *@(   #@/          *@#                       (@@%#####################%@&\n";
  print_string "     @@    @@@@/        .@@                    %@@%####&@##################%@&\n";
  print_string "    #@.    @@@%&@(       @@               ,&@@&%####&@@&%##################%@&\n";
  print_string "    @&     @&@@%#&@&,    @@.          /@@@%#####%&@@&%##############%&%####%@@\n";
  print_string "    @#     @&#&@&###%@@@@@@#///(&@@@@&%#####%&@@@%##################%@&####%@@\n";
  print_string "   *@*     @&###&@@%########%%%########%&@@@&%#######%###############@@%###%@@\n";
  print_string "   /@*    .@%#####%&@%#############&@@@&%###########@@%#############%@@%###%@@\n";
  print_string "   /@,    @@########################################&@%############%@@@@###%@&\n";
  print_string "   *@*   .@%################%@@%####################@@%############&@%@@###&@(\n";
  print_string ",@@@@@#  @&#########%@%#####@@%####################%@&############%@@%@@###&@/\n";
  print_string "@@@@@@@@@@%########%@&#####&@%%@&#################%@@%###########%@@%&@%##%@% \n";
  print_string ".@@@@@@/ @&#######&@&#####@@%%@@%################%@@%############@@%&@&##%@@  \n";
  print_string "          (@@&%#%@@%######%##@@%################%@@############%@@%%@&#%@@@&  \n";
  print_string "              #@@@@%#######%@@%################&@@%#############%#&@@@@&.     \n";
  print_string "                   ,%@@@@&%%###################&%#############%%&@@&/         \n";
  print_string "                           /%@@@@@@@&&%%####%#%%%###%%&@@@@@@@&/            . \n";
  print_string "                                      ,(&@@&%/**%&&&(,                        \n"

let time b f x =
  let t = Sys.time() in
  let fx = f x in
  if not b then Printf.printf "Execution time: %fs\n--\n\n" (Sys.time() -. t);
  fx

let print_condition str b =
  if not b then Printf.printf str else ()

let main () =
  let hide = Array.mem "-hide" Sys.argv in
  let noinfo = Array.mem "-noinfo" Sys.argv in
  let nofnc = Array.mem "-nofnc" Sys.argv in
  let tofile, i =
    match Array.find_index (fun x -> x = "-tofile") Sys.argv with
    | Some k -> (true, k)
    | None -> (false, 0)
  in
  let tofilepath = if tofile then Sys.argv.(i + 1) else "" in
  let version =
    if Array.mem "-naif" Sys.argv then 0
    else if Array.mem "-q1" Sys.argv then 1
    else if Array.mem "-q2" Sys.argv then 2
    else if Array.mem "-q3" Sys.argv then 3
    else if Array.mem "-q4" Sys.argv then 4
    else -1
  in

  match Sys.argv.(1) with
  | "help" -> help ()
  | "test" -> test ()
  | "fred" -> fred ()
  | _ ->
      print_condition "Reading file...\n%!" noinfo;
      let str = read_file Sys.argv.(1) in
      print_condition "Parsing...\n%!" noinfo;
      let formule = parse str in
      print_condition "Done.\n\n" noinfo;
      if not noinfo then 
        (Printf.printf "%d operators, %d variables\n" 
          (compte_ops formule) 
          (List.length (liste_var formule)));
      let fnc = if nofnc then false else (print_condition "Testing for cnf...\n%!" noinfo; est_fnc formule) in
      let result =
        if fnc then (print_condition "Using quine fnc\n%!" noinfo; time noinfo quine_fnc formule)
        else
          match version with
          | 0 -> print_condition "Using Naif\n%!" noinfo; time noinfo satsolver_naif formule
          | 1 -> print_condition "Using quine v1\n%!" noinfo; time noinfo quine formule
          | 2 -> print_condition "Using quine v2\n%!" noinfo; time noinfo quine_v2 formule
          | 3 -> print_condition "Using quine v3\n%!" noinfo; time noinfo quine_v3 formule
          | 4 -> print_condition "Using quine v4\n%!" noinfo; time noinfo quine_v4 formule
          | _ -> print_condition "Using quine v1\n%!" noinfo; time noinfo quine formule (*Solveur par defaut*)
      in
      if hide then () else print_true result;
      if tofile then write_file tofilepath result else ()

(* exécution de la fonction main *)
let _ = main ()
(*
 *)