type formule =
  | Var of string
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

(*compte ops f renvoie le nombre d'opérateurs utilisés dans s*)
let rec compte_ops (f : formule) : int =
  match f with
  | Var _ | Top | Bot -> 0
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
    match (f1, vars) with
    | Top, _ -> Some (valuation_init vars)
    | Bot, _ -> None
    | phi, x :: q -> (
        match aux (simpl_full (subst phi x Top)) q with
        | Some v -> Some ((x, true) :: v)
        | None -> (
            match aux (simpl_full (subst phi x Bot)) q with
            | Some v -> Some ((x, false) :: v)
            | None -> None))
    | _ -> failwith "impossible"
  in
  aux (simpl_full f) (liste_var f)

(*test quine v2*)
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
  match (f1, vars) with
  | Top, _ -> Some (valuation_init vars)
  | Bot, _ -> None
  | phi, x :: q -> (
      let f1, b1 = simpl_full_compt (subst phi x Top) in
      let f2, b2 = simpl_full_compt (subst phi x Bot) in
      match b1>b2 with
      | true -> (
        match aux f1 q with
        | Some v -> Some ((x, true) :: v)
        | None -> (
            match aux f2 q with
            | Some v -> Some ((x, false) :: v)
            | None -> None))
      | false -> (
        match aux f2 q with
        | Some v -> Some ((x, false) :: v)
        | None -> (
            match aux f1 q with
            | Some v -> Some ((x, true) :: v)
            | None -> None)))
  | _ -> failwith "impossible"
  in
  aux (simpl_full f) (liste_var f)
(*test quine v2*)

let rec print_true (v : valuation) : unit =
  match v with
  | [] -> ()
  | (x, true) :: q ->
      print_string x;
      print_newline ();
      print_true q
  | (x, false) :: q -> print_true 



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
  assert (quine (parse "x|y") = Some [ ("x", true); ("y", false) ]);
  assert (quine (parse "x & y | y & ~x") = Some [ ("x", true); ("y", true) ]);
  assert (
    quine (parse "a|b|c|d|e|f|g|h")
    = Some
        [
          ("a", true);
          ("b", false);
          ("c", false);
          ("d", false);
          ("e", false);
          ("f", false);
          ("g", false);
          ("h", false);
        ])

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

  print_string "Tous les tests ont reussi\n"

let main () =
  match Sys.argv with
  | [| _ |] -> failwith "pas d'urguments"
  | _ -> (
      if Sys.argv.(1) = "test" then test ()
      else
        let str = read_file Sys.argv.(1) in
        let f = parse str in
        let res = quine_v2 f in
        match res with
        | None -> print_string "Pas de solution\n"
        | Some v ->
            print_string
              "La formule est satisfiable en assignant 1 aux variables \
               suivantes et 0 aux autres\n";
            print_true v)

(* exécution de la fonction main *)
let _ = main ()
(* *)
