open Hashtbl
open Printf

let create_hash (l: string list) = 
  let tovar = Hashtbl.create 300 in
  let toid = Hashtbl.create 300 in

  let rec fill_hash l1 n : unit = match l1 with
    | [] -> ()
    | x::q -> Hashtbl.add tovar n x;
              Hashtbl.add toid x n;
              fill_hash q (n+1)
  in
  fill_hash l 0;
  (tovar, toid)

let main () = 
  let tovar, toid  = create_hash ["x"; "y"; "z"] in
  ()

let _ = main ()