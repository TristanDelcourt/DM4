
# 1 "type.ml"
(** {3 Type of the JSON tree} *)

type t =
    [
    | `Null
    | `Bool of bool
    
# 8 "type.ml"
    | `Int of int
    
# 14 "type.ml"
    | `Float of float
    
# 20 "type.ml"
    | `String of string
    
# 25 "type.ml"
    | `Assoc of (string * t) list
    | `List of t list
    
# 33 "type.ml"
    ]
(**
All possible cases defined in Yojson:
- `Null: JSON null
- `Bool of bool: JSON boolean
- `Int of int: JSON number without decimal point or exponent.
- `Intlit of string: JSON number without decimal point or exponent,
	    preserved as a string.
- `Float of float: JSON number, Infinity, -Infinity or NaN.
- `Floatlit of string: JSON number, Infinity, -Infinity or NaN,
	    preserved as a string.
- `String of string: JSON string. Bytes in the range 128-255 are preserved
	    as-is without encoding validation for both reading
	    and writing.
- `Stringlit of string: JSON string literal including the double quotes.
- `Assoc of (string * json) list: JSON object.
- `List of json list: JSON array.
- `Tuple of json list: Tuple (non-standard extension of JSON).
	    Syntax: [("abc", 123)].
- `Variant of (string * json option): Variant (non-standard extension of JSON).
	    Syntax: [<"Foo">] or [<"Bar":123>].
*)

(*
  Note to adventurers: ocamldoc does not support inline comments
  on each polymorphic variant, and cppo doesn't allow to concatenate
  comments, so it would be complicated to document only the
  cases that are preserved by cppo in the type definition.
*)

# 1 "write.ml"
let hex n =
  Char.chr (
    if n < 10 then n + 48
    else n + 87
  )

let write_special src start stop ob str =
  Buffer.add_substring ob src !start (stop - !start);
  Buffer.add_string ob str;
  start := stop + 1

let write_control_char src start stop ob c =
  Buffer.add_substring ob src !start (stop - !start);
  Buffer.add_string ob "\\u00";
  Buffer.add_char ob (hex (Char.code c lsr 4));
  Buffer.add_char ob (hex (Char.code c land 0xf));
  start := stop + 1

let finish_string src start ob =
  try
    Buffer.add_substring ob src !start (String.length src - !start)
  with exc ->
    Printf.eprintf "src=%S start=%i len=%i\n%!"
      src !start (String.length src - !start);
    raise exc

let write_string_body ob s =
  let start = ref 0 in
  for i = 0 to String.length s - 1 do
    match s.[i] with
        '"' -> write_special s start i ob "\\\""
      | '\\' -> write_special s start i ob "\\\\"
      | '\b' -> write_special s start i ob "\\b"
      | '\012' -> write_special s start i ob "\\f"
      | '\n' -> write_special s start i ob "\\n"
      | '\r' -> write_special s start i ob "\\r"
      | '\t' -> write_special s start i ob "\\t"
      | '\x00'..'\x1F'
      | '\x7F' as c -> write_control_char s start i ob c
      | _ -> ()
  done;
  finish_string s start ob

let write_string ob s =
  Buffer.add_char ob '"';
  write_string_body ob s;
  Buffer.add_char ob '"'

let json_string_of_string s =
  let ob = Buffer.create 10 in
  write_string ob s;
  Buffer.contents ob

let write_null ob () =
  Buffer.add_string ob "null"

let write_bool ob x =
  Buffer.add_string ob (if x then "true" else "false")

let dec n =
  Char.chr (n + 48)

let rec write_digits s x =
  if x = 0 then ()
  else
    let d = x mod 10 in
    write_digits s (x / 10);
    Buffer.add_char s (dec (abs d))

let write_int ob x =
  if x > 0 then
    write_digits ob x
  else if x < 0 then (
    Buffer.add_char ob '-';
    write_digits ob x
  )
  else
    Buffer.add_char ob '0'


let json_string_of_int i =
  string_of_int i


(*
  Ensure that the float is not printed as an int.
  This is not required by JSON, but useful in order to guarantee
  reversibility.
*)
let float_needs_period s =
  try
    for i = 0 to String.length s - 1 do
      match s.[i] with
          '0'..'9' | '-' -> ()
        | _ -> raise Exit
    done;
    true
  with Exit ->
    false

(*
  Guarantees that a sufficient number of digits are printed in order to allow
  reversibility.
*)
let write_float ob x =
  match classify_float x with
    FP_nan ->
      Buffer.add_string ob "NaN"
  | FP_infinite ->
      Buffer.add_string ob (if x > 0. then "Infinity" else "-Infinity")
  | _ ->
      let s1 = Printf.sprintf "%.16g" x in
      let s =
        if float_of_string s1 = x then s1
        else Printf.sprintf "%.17g" x
      in
      Buffer.add_string ob s;
      if float_needs_period s then
        Buffer.add_string ob ".0"

let write_normal_float_prec significant_figures ob x =
  let open Printf in
  let s =
    match significant_figures with
        1 -> sprintf "%.1g" x
      | 2 -> sprintf "%.2g" x
      | 3 -> sprintf "%.3g" x
      | 4 -> sprintf "%.4g" x
      | 5 -> sprintf "%.5g" x
      | 6 -> sprintf "%.6g" x
      | 7 -> sprintf "%.7g" x
      | 8 -> sprintf "%.8g" x
      | 9 -> sprintf "%.9g" x
      | 10 -> sprintf "%.10g" x
      | 11 -> sprintf "%.11g" x
      | 12 -> sprintf "%.12g" x
      | 13 -> sprintf "%.13g" x
      | 14 -> sprintf "%.14g" x
      | 15 -> sprintf "%.15g" x
      | 16 -> sprintf "%.16g" x
      | _ -> sprintf "%.17g" x
  in
  Buffer.add_string ob s;
  if float_needs_period s then
    Buffer.add_string ob ".0"

(* used by atdgen *)
let write_float_prec significant_figures ob x =
  match classify_float x with
    FP_nan ->
      Buffer.add_string ob "NaN"
  | FP_infinite ->
      Buffer.add_string ob (if x > 0. then "Infinity" else "-Infinity")
  | _ ->
      write_normal_float_prec significant_figures ob x

let json_string_of_float x =
  let ob = Buffer.create 20 in
  write_float ob x;
  Buffer.contents ob


let write_std_float ob x =
  match classify_float x with
    FP_nan ->
      Common.json_error "NaN value not allowed in standard JSON"
  | FP_infinite ->
      Common.json_error
        (if x > 0. then
           "Infinity value not allowed in standard JSON"
         else
           "-Infinity value not allowed in standard JSON")
  | _ ->
      let s1 = Printf.sprintf "%.16g" x in
      let s =
        if float_of_string s1 = x then s1
        else Printf.sprintf "%.17g" x
      in
      Buffer.add_string ob s;
      if float_needs_period s then
        Buffer.add_string ob ".0"

(* used by atdgen *)
let write_std_float_prec significant_figures ob x =
  match classify_float x with
    FP_nan ->
      Common.json_error "NaN value not allowed in standard JSON"
  | FP_infinite ->
      Common.json_error
        (if x > 0. then
           "Infinity value not allowed in standard JSON"
         else
           "-Infinity value not allowed in standard JSON")
  | _ ->
      write_normal_float_prec significant_figures ob x

let std_json_string_of_float x =
  let ob = Buffer.create 20 in
  write_std_float ob x;
  Buffer.contents ob


let write_intlit = Buffer.add_string
let write_floatlit = Buffer.add_string
let write_stringlit = Buffer.add_string

let rec iter2_aux f_elt f_sep x = function
    [] -> ()
  | y :: l ->
      f_sep x;
      f_elt x y;
      iter2_aux f_elt f_sep x l

let iter2 f_elt f_sep x = function
    [] -> ()
  | y :: l ->
      f_elt x y;
      iter2_aux f_elt f_sep x l

let f_sep ob =
  Buffer.add_char ob ','

let rec write_json ob (x : t) =
  match x with
      `Null -> write_null ob ()
    | `Bool b -> write_bool ob b
    
# 228 "write.ml"
    | `Int i -> write_int ob i
    
# 234 "write.ml"
    | `Float f -> write_float ob f
    
# 240 "write.ml"
    | `String s -> write_string ob s
    
# 245 "write.ml"
    | `Assoc l -> write_assoc ob l
    | `List l -> write_list ob l

# 254 "write.ml"
and write_assoc ob l =
  let f_elt ob (s, x) =
    write_string ob s;
    Buffer.add_char ob ':';
    write_json ob x
  in
  Buffer.add_char ob '{';
  iter2 f_elt f_sep ob l;
  Buffer.add_char ob '}';

and write_list ob l =
  Buffer.add_char ob '[';
  iter2 write_json f_sep ob l;
  Buffer.add_char ob ']'



# 289 "write.ml"
let write_t = write_json

let rec write_std_json ob (x : t) =
  match x with
      `Null -> write_null ob ()
    | `Bool b -> write_bool ob b
    
# 296 "write.ml"
    | `Int i -> write_int ob i
    
# 302 "write.ml"
    | `Float f -> write_std_float ob f
    
# 308 "write.ml"
    | `String s -> write_string ob s
    
# 313 "write.ml"
    | `Assoc l -> write_std_assoc ob l
    | `List l -> write_std_list ob l

# 322 "write.ml"
and write_std_assoc ob l =
  let f_elt ob (s, x) =
    write_string ob s;
    Buffer.add_char ob ':';
    write_std_json ob x
  in
  Buffer.add_char ob '{';
  iter2 f_elt f_sep ob l;
  Buffer.add_char ob '}';

and write_std_list ob l =
  Buffer.add_char ob '[';
  iter2 write_std_json f_sep ob l;
  Buffer.add_char ob ']'

and write_std_tuple ob l =
  Buffer.add_char ob '[';
  iter2 write_std_json f_sep ob l;
  Buffer.add_char ob ']'



# 355 "write.ml"
let to_buffer ?(suf = "") ?(std = false) ob x =
  if std then
    write_std_json ob x
  else
    write_json ob x;
  Buffer.add_string ob suf

let to_string ?buf ?(len = 256) ?(suf = "") ?std x =
  let ob =
    match buf with
        None -> Buffer.create len
      | Some ob ->
          Buffer.clear ob;
          ob
  in
  to_buffer ~suf ?std ob x;
  let s = Buffer.contents ob in
  Buffer.clear ob;
  s

let to_channel ?buf ?(len=4096) ?(suf = "") ?std oc x =
  let ob =
    match buf with
        None -> Buffer.create len
      | Some ob -> Buffer.clear ob; ob
  in
  to_buffer ~suf ?std ob x;
  Buffer.output_buffer oc ob;
  Buffer.clear ob

let to_output ?buf ?(len=4096) ?(suf = "") ?std out x =
  let ob =
    match buf with
        None -> Buffer.create len
      | Some ob -> Buffer.clear ob; ob
  in
  to_buffer ~suf ?std ob x;
  (* this requires an int and never uses it. This is done to preserve
     backward compatibility to not break the signatur but can safely
     be changed to require unit in a future compatibility-breaking
     release *)
  let _ : int = out#output (Buffer.contents ob) 0 (Buffer.length ob) in
  Buffer.clear ob

let to_file ?len ?std ?(suf = "\n") file x =
  let oc = open_out file in
  try
    to_channel ?len ~suf ?std oc x;
    close_out oc
  with e ->
    close_out_noerr oc;
    raise e

let seq_to_buffer ?(suf = "\n") ?std ob st =
  Seq.iter (to_buffer ~suf ?std ob) st

let seq_to_string ?buf ?(len = 256) ?(suf = "\n") ?std st =
  let ob =
    match buf with
        None -> Buffer.create len
      | Some ob ->
          Buffer.clear ob;
          ob
  in
  seq_to_buffer ~suf ?std ob st;
  let s = Buffer.contents ob in
  Buffer.clear ob;
  s

let seq_to_channel ?buf ?(len=2096) ?(suf = "\n") ?std oc seq =
  let ob =
    match buf with
        None -> Buffer.create len
      | Some ob -> Buffer.clear ob; ob
  in
  Seq.iter (fun json ->
    to_buffer ~suf ?std ob json;
    Buffer.output_buffer oc ob;
    Buffer.clear ob;
  ) seq

let seq_to_file ?len ?(suf = "\n") ?std file st =
  let oc = open_out file in
  try
    seq_to_channel ?len ~suf ?std oc st;
    close_out oc
  with e ->
    close_out_noerr oc;
    raise e


let rec sort = function
  | `Assoc l ->
      let l = List.rev (List.rev_map (fun (k, v) -> (k, sort v)) l) in
      `Assoc (List.stable_sort (fun (a, _) (b, _) -> String.compare a b) l)
  | `List l ->
      `List (List.rev (List.rev_map sort l))
  
# 463 "write.ml"
  | x -> x

# 9 "basic.cppo.ml"
module Pretty = struct
# 1 "prettyprint.ml"
(*
   Pretty-print JSON data in an attempt to maximize readability.

   1. What fits on one line stays on one line.
   2. What doesn't fit on one line gets printed more vertically so as to not
      exceed a reasonable page width, if possible.

   Arrays containing only simple elements ("atoms") are pretty-printed with
   end-of-line wrapping like ordinary text:

     [
        "hello", "hello", "hello", "hello", "hello", "hello", "hello", "hello",
        "hello", "hello", "hello", "hello", "hello", "hello", "hello", "hello"
     ]

   Other arrays are printed either horizontally or vertically depending
   on whether they fit on a single line:

     [ { "hello": "world" }, { "hello": "world" }, { "hello": "world" } ]

   or

     [
       { "hello": "world" },
       { "hello": "world" },
       { "hello": "world" },
       { "hello": "world" }
     ]
*)

let pp_list sep ppx out l =
  let pp_sep out () = Format.fprintf out "%s@ " sep in
  Format.pp_print_list ~pp_sep ppx out l

let is_atom (x: [> t]) =
  match x with
  | `Null
  | `Bool _
  | `Int _
  | `Float _
  | `String _
  | `Intlit _
  | `Floatlit _
  | `Stringlit _
  | `List []
  | `Assoc []
  | `Tuple []
  | `Variant (_, None) -> true
  | `List _
  | `Assoc _
  | `Tuple _
  | `Variant (_, Some _) -> false

let is_atom_list l =
  List.for_all is_atom l

(*
   inside_box: indicates that we're already within a box that imposes
   a certain style and we shouldn't create a new one. This is used for
   printing field values like this:

     foo: [
       bar
     ]

   rather than something else like

     foo:
       [
         bar
       ]
*)
let rec format ~inside_box std (out:Format.formatter) (x:t) : unit =
  match x with
    | `Null -> Format.pp_print_string out "null"
    | `Bool x -> Format.pp_print_bool out x
    
# 78 "prettyprint.ml"
    | `Int x -> Format.pp_print_string out (json_string_of_int x)
    
# 81 "prettyprint.ml"
    | `Float x ->
        let s =
          if std then std_json_string_of_float x
          else json_string_of_float x
        in
        Format.pp_print_string out s
    
# 89 "prettyprint.ml"
    | `String s -> Format.pp_print_string out (json_string_of_string s)
    
# 100 "prettyprint.ml"
    | `List [] -> Format.pp_print_string out "[]"
    | `List l ->
      if not inside_box then Format.fprintf out "@[<hv2>";
      if is_atom_list l then
        (* use line wrapping like we would do for a paragraph of text *)
        Format.fprintf out "[@;<1 0>@[<hov>%a@]@;<1 -2>]"
          (pp_list "," (format ~inside_box:false std)) l
      else
        (* print the elements horizontally if they fit on the line,
           otherwise print them in a column *)
        Format.fprintf out "[@;<1 0>@[<hv>%a@]@;<1 -2>]"
          (pp_list "," (format ~inside_box:false std)) l;
      if not inside_box then Format.fprintf out "@]";
    | `Assoc [] -> Format.pp_print_string out "{}"
    | `Assoc l ->
      if not inside_box then Format.fprintf out "@[<hv2>";
      Format.fprintf out "{@;<1 0>%a@;<1 -2>}" (pp_list "," (format_field std)) l;
      if not inside_box then Format.fprintf out "@]";

# 156 "prettyprint.ml"
and format_field std out (name, x) =
  Format.fprintf out "@[<hv2>%s: %a@]" (json_string_of_string name) (format ~inside_box:true std) x

let pp ?(std = false) out x =
  Format.fprintf out "@[<hv2>%a@]" (format ~inside_box:true std) (x :> t)

let to_string ?std x =
  Format.asprintf "%a" (pp ?std) x

let to_channel ?std oc x =
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "%a@?" (pp ?std) x
# 11 "basic.cppo.ml"
end

# 1 "monomorphic.ml"
let rec pp fmt =
  function
  | `Null -> Format.pp_print_string fmt "`Null"
  | `Bool x ->
    Format.fprintf fmt "`Bool (@[<hov>";
    Format.fprintf fmt "%B" x;
    Format.fprintf fmt "@])"
  
# 9 "monomorphic.ml"
  | `Int x ->
    Format.fprintf fmt "`Int (@[<hov>";
    Format.fprintf fmt "%d" x;
    Format.fprintf fmt "@])"
  
# 21 "monomorphic.ml"
  | `Float x ->
    Format.fprintf fmt "`Float (@[<hov>";
    Format.fprintf fmt "%F" x;
    Format.fprintf fmt "@])"
  
# 33 "monomorphic.ml"
  | `String x ->
    Format.fprintf fmt "`String (@[<hov>";
    Format.fprintf fmt "%S" x;
    Format.fprintf fmt "@])"
  
# 44 "monomorphic.ml"
  | `Assoc xs ->
    Format.fprintf fmt "`Assoc (@[<hov>";
    Format.fprintf fmt "@[<2>[";
    ignore (List.fold_left
      (fun sep (key, value) ->
        if sep then
          Format.fprintf fmt ";@ ";
          Format.fprintf fmt "(@[";
          Format.fprintf fmt "%S" key;
          Format.fprintf fmt ",@ ";
          pp fmt value;
          Format.fprintf fmt "@])";
          true) false xs);
    Format.fprintf fmt "@,]@]";
    Format.fprintf fmt "@])"
  | `List xs ->
    Format.fprintf fmt "`List (@[<hov>";
    Format.fprintf fmt "@[<2>[";
    ignore (List.fold_left
      (fun sep x ->
        if sep then
          Format.fprintf fmt ";@ ";
          pp fmt x;
          true) false xs);
    Format.fprintf fmt "@,]@]";
    Format.fprintf fmt "@])"

# 99 "monomorphic.ml"
let show x =
  Format.asprintf "%a" pp x

let rec equal a b =
  match a, b with
  | `Null, `Null -> true
  | `Bool a, `Bool b -> a = b
  
# 107 "monomorphic.ml"
  | `Int a, `Int b -> a = b
    
# 113 "monomorphic.ml"
    | `Float a, `Float b -> a = b
    
# 119 "monomorphic.ml"
    | `String a, `String b -> a = b
    
# 124 "monomorphic.ml"
    | `Assoc xs, `Assoc ys ->
      let compare_keys = fun (key, _) (key', _) -> String.compare key key' in
      let xs = List.stable_sort compare_keys xs in
      let ys = List.stable_sort compare_keys ys in
      (match List.for_all2 (fun (key, value) (key', value') ->
        match key = key' with
        | false -> false
        | true -> equal value value') xs ys with
      | result -> result
      | exception Invalid_argument _ ->
        (* the lists were of different lengths, thus unequal *)
        false)
    
# 139 "monomorphic.ml"
    | `List xs, `List ys ->
      (match List.for_all2 equal xs ys with
      | result -> result
      | exception Invalid_argument _ ->
        (* the lists were of different lengths, thus unequal *)
        false)
    
# 155 "monomorphic.ml"
    | _ -> false

# 1 "write2.ml"
let pretty_print ?std out x = Pretty.pp ?std out x
let pretty_to_string ?std x = Pretty.to_string ?std x
let pretty_to_channel ?std oc x = Pretty.to_channel ?std oc x


# 1 "lib/read.mll"
 
  
# 2 "lib/read.mll"
  module Lexing =
    (*
      We override Lexing.engine in order to avoid creating a new position
      record each time a rule is matched.
      This reduces total parsing time by about 31%.
    *)
  struct
    include Lexing

    external c_engine : lex_tables -> int -> lexbuf -> int = "caml_lex_engine"

    let engine tbl state buf =
      let result = c_engine tbl state buf in
      (*
      if result >= 0 then begin
        buf.lex_start_p <- buf.lex_curr_p;
        buf.lex_curr_p <- {buf.lex_curr_p
                           with pos_cnum = buf.lex_abs_pos + buf.lex_curr_pos};
      end;
      *)
      result
  end

  open Printf
  open Lexing

  (* see description in common.mli *)
  type lexer_state = Common.Lexer_state.t = {
    buf : Buffer.t;
    mutable lnum : int;
    mutable bol : int;
    mutable fname : string option;
  }

  let dec c =
    Char.code c - 48

  let hex c =
    match c with
        '0'..'9' -> int_of_char c - int_of_char '0'
      | 'a'..'f' -> int_of_char c - int_of_char 'a' + 10
      | 'A'..'F' -> int_of_char c - int_of_char 'A' + 10
      | _ -> assert false

  let custom_error descr v lexbuf =
    let offs = lexbuf.lex_abs_pos - 1 in
    let bol = v.bol in
    let pos1 = offs + lexbuf.lex_start_pos - bol - 1 in
    let pos2 = max pos1 (offs + lexbuf.lex_curr_pos - bol) in
    let file_line =
      match v.fname with
          None -> "Line"
        | Some s ->
            sprintf "File %s, line" s
    in
    let bytes =
      if pos1 = pos2 then
        sprintf "byte %i" (pos1+1)
      else
        sprintf "bytes %i-%i" (pos1+1) (pos2+1)
    in
    let msg = sprintf "%s %i, %s:\n%s" file_line v.lnum bytes descr in
    Common.json_error msg


  let lexer_error descr v lexbuf =
    custom_error
      (sprintf "%s '%s'" descr (Lexing.lexeme lexbuf))
      v lexbuf

  let long_error descr v lexbuf =
    let junk = Lexing.lexeme lexbuf in
    let buf_size = 32 in
    let buf = Buffer.create buf_size in
    let () = Lexer_utils.read_junk_without_positions buf buf_size lexbuf in
    let extra_junk = Buffer.contents buf in
    custom_error
      (sprintf "%s '%s%s'" descr junk extra_junk)
      v lexbuf

  let min10 = min_int / 10 - (if min_int mod 10 = 0 then 0 else 1)
  let max10 = max_int / 10 + (if max_int mod 10 = 0 then 0 else 1)

  exception Int_overflow

  let extract_positive_int lexbuf =
    let start = lexbuf.lex_start_pos in
    let stop = lexbuf.lex_curr_pos in
    let s = lexbuf.lex_buffer in
    let n = ref 0 in
    for i = start to stop - 1 do
      if !n >= max10 then
        raise Int_overflow
      else
        n := 10 * !n + dec (Bytes.get s i)
    done;
    if !n < 0 then
      raise Int_overflow
    else
      !n

  let make_positive_int v lexbuf =
      
# 105 "lib/read.mll"
      try `Int (extract_positive_int lexbuf)
      with Int_overflow ->
        
# 111 "lib/read.mll"
        lexer_error "Int overflow" v lexbuf

  
# 114 "lib/read.mll"
  let extract_negative_int lexbuf =
    let start = lexbuf.lex_start_pos + 1 in
    let stop = lexbuf.lex_curr_pos in
    let s = lexbuf.lex_buffer in
    let n = ref 0 in
    for i = start to stop - 1 do
      if !n <= min10 then
        raise Int_overflow
      else
        n := 10 * !n - dec (Bytes.get s i)
    done;
    if !n > 0 then
      raise Int_overflow
    else
      !n

  let make_negative_int v lexbuf =
      
# 132 "lib/read.mll"
      try `Int (extract_negative_int lexbuf)
      with Int_overflow ->
        
# 138 "lib/read.mll"
        lexer_error "Int overflow" v lexbuf

  
# 141 "lib/read.mll"
  let newline v lexbuf =
    v.lnum <- v.lnum + 1;
    v.bol <- lexbuf.lex_abs_pos + lexbuf.lex_curr_pos

  let add_lexeme buf lexbuf =
    let len = lexbuf.lex_curr_pos - lexbuf.lex_start_pos in
    Buffer.add_subbytes buf lexbuf.lex_buffer lexbuf.lex_start_pos len

  let map_lexeme f lexbuf =
    let len = lexbuf.lex_curr_pos - lexbuf.lex_start_pos in
    f (Bytes.sub_string lexbuf.lex_buffer lexbuf.lex_start_pos len) 0 len

  type variant_kind = [ `Edgy_bracket | `Square_bracket | `Double_quote ]


# 157 "lib/read.ml"
# 157 "lib/read.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\236\255\237\255\003\000\239\255\016\000\242\255\243\255\
    \244\255\245\255\000\000\031\000\249\255\085\000\001\000\000\000\
    \000\000\001\000\000\000\001\000\002\000\255\255\000\000\000\000\
    \003\000\254\255\001\000\004\000\253\255\011\000\252\255\003\000\
    \001\000\003\000\002\000\003\000\000\000\251\255\021\000\097\000\
    \010\000\022\000\020\000\016\000\022\000\012\000\008\000\250\255\
    \119\000\129\000\139\000\161\000\171\000\181\000\193\000\209\000\
    \240\255\011\000\038\000\252\255\065\000\254\255\255\255\110\000\
    \252\255\163\000\254\255\255\255\234\000\247\255\248\255\048\001\
    \250\255\251\255\252\255\253\255\254\255\255\255\071\001\126\001\
    \149\001\249\255\039\000\253\255\254\255\038\000\187\001\210\001\
    \248\001\015\002\255\255\220\000\253\255\255\255\245\000\039\002\
    \109\002\014\001\088\002\164\002\187\002\225\002\013\000\252\255\
    \253\255\254\255\255\255\014\000\253\255\254\255\255\255\030\000\
    \253\255\254\255\255\255\015\000\253\255\254\255\255\255\017\001\
    \251\255\252\255\253\255\254\255\255\255\019\000\252\255\253\255\
    \254\255\015\000\255\255\016\000\255\255\008\001\005\000\253\255\
    \023\000\254\255\020\000\255\255\046\000\253\255\254\255\042\000\
    \052\000\053\000\255\255\053\000\048\000\091\000\092\000\255\255\
    \027\001\250\255\251\255\137\000\104\000\089\000\088\000\106\000\
    \255\255\143\000\137\000\177\000\254\255\183\000\168\000\166\000\
    \183\000\002\000\253\255\177\000\172\000\187\000\004\000\252\255\
    \053\002\251\255\252\255\253\255\103\001\255\255\248\002\254\255\
    \006\003\030\003\252\255\253\255\254\255\255\255\040\003\050\003\
    \074\003\252\255\253\255\254\255\255\255\061\003\084\003\108\003\
    \249\255\250\255\251\255\244\000\120\003\142\003\179\000\194\000\
    \015\000\255\255\190\000\188\000\187\000\193\000\183\000\179\000\
    \254\255\191\000\201\000\200\000\196\000\203\000\193\000\189\000\
    \253\255\157\003\095\003\174\003\196\003\206\003\216\003\228\003\
    \239\003\060\000\253\255\254\255\255\255\012\004\252\255\253\255\
    \087\004\255\255\145\004\252\255\253\255\221\004\255\255\229\000\
    \253\255\254\255\255\255\231\000\253\255\254\255\255\255\002\000\
    \255\255\018\001\252\255\253\255\254\255\255\255\034\001\253\255\
    \254\255\255\255\000\000\255\255\003\000\254\255\255\255\038\001\
    \252\255\253\255\254\255\255\255\120\001\251\255\252\255\253\255\
    \254\255\255\255\208\000\253\255\254\255\255\255\211\000\253\255\
    \254\255\255\255\189\000\255\255\143\001\252\255\253\255\254\255\
    \255\255\013\001\253\255\254\255\255\255\095\001\252\255\253\255\
    \254\255\255\255\050\001\253\255\254\255\255\255\026\001\253\255\
    \254\255\255\255\233\000\253\255\254\255\255\255\222\000\253\255\
    \254\255\255\255\079\005\237\255\238\255\010\000\240\255\044\001\
    \243\255\244\255\245\255\246\255\061\001\002\004\249\255\045\005\
    \209\000\228\000\211\000\232\000\225\000\223\000\240\000\255\255\
    \235\000\234\000\008\001\254\255\004\001\023\001\253\255\054\001\
    \252\255\031\001\029\001\032\001\039\001\049\001\045\001\251\255\
    \057\001\082\001\080\001\078\001\084\001\074\001\086\001\250\255\
    \110\005\012\004\123\005\155\005\165\005\177\005\187\005\197\005\
    \241\255\199\001\077\002\253\255\255\255\154\002\222\005\209\005\
    \155\002\239\005\053\006\076\006\114\006\016\002\252\255\253\255\
    \254\255\255\255\152\006\252\255\253\255\227\006\255\255\085\007\
    \244\255\245\255\011\000\247\255\076\002\250\255\251\255\252\255\
    \253\255\254\255\031\002\243\005\051\007\100\001\115\001\104\001\
    \133\001\118\001\154\001\171\001\255\255\173\001\176\001\191\001\
    \185\001\187\001\253\001\230\001\230\001\234\001\247\001\237\001\
    \234\001\009\002\019\002\019\002\015\002\021\002\011\002\007\002\
    \142\006\152\006\116\007\170\007\180\007\190\007\200\007\210\007\
    \248\255\120\002\167\002\253\255\255\255\216\002\082\007\220\007\
    \236\002\244\007\058\008\081\008\119\008\076\002\252\255\253\255\
    \254\255\255\255\157\008\252\255\253\255\232\008\255\255\135\002\
    \120\002\253\255\100\002\254\255\182\002\255\255\011\002\255\255\
    \204\002\252\255\253\255\254\255\255\255\046\002\255\255\178\002\
    \252\255\253\255\254\255\255\255\023\000\255\255\183\002\252\255\
    \253\255\254\255\255\255\187\002\253\255\254\255\255\255\121\002\
    \253\255\254\255\255\255\184\002\252\255\253\255\254\255\019\000\
    \255\255";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\017\000\255\255\019\000\255\255\255\255\
    \255\255\255\255\007\000\007\000\255\255\019\000\019\000\019\000\
    \019\000\019\000\019\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\008\000\008\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\009\000\255\255\009\000\255\255\009\000\255\255\
    \255\255\014\000\255\255\255\255\002\000\255\255\255\255\255\255\
    \255\255\002\000\255\255\255\255\255\255\255\255\255\255\007\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\001\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\001\000\001\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\003\000\255\255\001\000\255\255\004\000\003\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\001\000\
    \255\255\255\255\255\255\001\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\004\000\004\000\004\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\003\000\255\255\000\000\255\255\
    \001\000\255\255\255\255\255\255\255\255\255\255\000\000\002\000\
    \255\255\255\255\255\255\255\255\255\255\000\000\002\000\255\255\
    \255\255\255\255\255\255\003\000\003\000\005\000\005\000\005\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\003\000\255\255\003\000\255\255\003\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \001\000\255\255\255\255\255\255\255\255\001\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\001\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\001\000\255\255\002\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\001\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\016\000\255\255\018\000\
    \255\255\255\255\255\255\255\255\007\000\007\000\255\255\018\000\
    \018\000\018\000\018\000\018\000\018\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\008\000\255\255\008\000\255\255\008\000\255\255\
    \255\255\013\000\255\255\255\255\255\255\001\000\001\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\001\000\255\255\255\255\
    \255\255\255\255\009\000\255\255\011\000\255\255\255\255\255\255\
    \255\255\255\255\000\000\000\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\255\255\000\000\255\255\000\000\255\255\
    \255\255\006\000\255\255\255\255\255\255\001\000\001\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\001\000\255\255\004\000\
    \003\000\255\255\255\255\255\255\255\255\255\255\001\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\001\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\001\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\003\000\
    \255\255";
  Lexing.lex_default =
   "\001\000\000\000\000\000\255\255\000\000\255\255\000\000\000\000\
    \000\000\000\000\255\255\255\255\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
    \255\255\000\000\255\255\255\255\000\000\255\255\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\057\000\060\000\000\000\060\000\000\000\000\000\065\000\
    \000\000\065\000\000\000\000\000\070\000\000\000\000\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\255\255\255\255\
    \255\255\000\000\084\000\000\000\000\000\255\255\255\255\255\255\
    \255\255\255\255\000\000\094\000\000\000\000\000\097\000\255\255\
    \255\255\097\000\255\255\255\255\255\255\255\255\104\000\000\000\
    \000\000\000\000\000\000\109\000\000\000\000\000\000\000\113\000\
    \000\000\000\000\000\000\117\000\000\000\000\000\000\000\121\000\
    \000\000\000\000\000\000\000\000\000\000\126\000\000\000\000\000\
    \000\000\255\255\000\000\255\255\000\000\255\255\255\255\000\000\
    \255\255\000\000\138\000\000\000\142\000\000\000\000\000\255\255\
    \255\255\255\255\000\000\255\255\255\255\255\255\255\255\000\000\
    \154\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
    \255\255\255\255\000\000\255\255\255\255\255\255\255\255\000\000\
    \178\000\000\000\000\000\000\000\255\255\000\000\255\255\000\000\
    \255\255\187\000\000\000\000\000\000\000\000\000\255\255\255\255\
    \194\000\000\000\000\000\000\000\000\000\255\255\255\255\201\000\
    \000\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\235\000\000\000\000\000\000\000\239\000\000\000\000\000\
    \255\255\000\000\244\000\000\000\000\000\255\255\000\000\249\000\
    \000\000\000\000\000\000\253\000\000\000\000\000\000\000\255\255\
    \000\000\003\001\000\000\000\000\000\000\000\000\008\001\000\000\
    \000\000\000\000\255\255\000\000\255\255\000\000\000\000\017\001\
    \000\000\000\000\000\000\000\000\022\001\000\000\000\000\000\000\
    \000\000\000\000\028\001\000\000\000\000\000\000\032\001\000\000\
    \000\000\000\000\255\255\000\000\038\001\000\000\000\000\000\000\
    \000\000\043\001\000\000\000\000\000\000\047\001\000\000\000\000\
    \000\000\000\000\052\001\000\000\000\000\000\000\056\001\000\000\
    \000\000\000\000\060\001\000\000\000\000\000\000\064\001\000\000\
    \000\000\000\000\067\001\000\000\000\000\255\255\000\000\255\255\
    \000\000\000\000\000\000\000\000\255\255\255\255\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\255\255\000\000\255\255\255\255\000\000\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\121\001\125\001\000\000\000\000\128\001\255\255\255\255\
    \128\001\255\255\255\255\255\255\255\255\135\001\000\000\000\000\
    \000\000\000\000\140\001\000\000\000\000\255\255\000\000\144\001\
    \000\000\000\000\255\255\000\000\255\255\000\000\000\000\000\000\
    \000\000\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\193\001\197\001\000\000\000\000\200\001\255\255\255\255\
    \200\001\255\255\255\255\255\255\255\255\207\001\000\000\000\000\
    \000\000\000\000\212\001\000\000\000\000\255\255\000\000\255\255\
    \255\255\000\000\255\255\000\000\220\001\000\000\255\255\000\000\
    \226\001\000\000\000\000\000\000\000\000\255\255\000\000\233\001\
    \000\000\000\000\000\000\000\000\255\255\000\000\240\001\000\000\
    \000\000\000\000\000\000\245\001\000\000\000\000\000\000\249\001\
    \000\000\000\000\000\000\252\001\000\000\000\000\000\000\255\255\
    \000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\003\000\004\000\000\000\003\000\003\000\134\000\000\000\
    \003\000\000\000\134\000\069\001\146\001\255\255\000\000\069\001\
    \146\001\000\000\000\000\000\000\000\000\127\000\139\000\000\000\
    \003\000\000\000\012\000\003\000\170\000\134\000\175\000\000\000\
    \007\000\011\001\069\001\146\001\014\001\013\000\049\000\005\000\
    \010\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\056\000\118\000\006\000\129\000\130\000\057\000\
    \237\001\137\000\000\002\049\000\000\000\048\000\138\000\106\000\
    \062\000\014\000\110\000\105\000\000\000\049\000\015\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\030\000\048\000\008\000\114\000\209\000\236\000\000\001\
    \013\001\029\000\022\000\255\255\048\000\048\000\017\000\021\000\
    \025\000\032\000\033\000\035\000\023\000\027\000\016\000\031\000\
    \028\000\034\000\019\000\024\000\018\000\026\000\020\000\036\000\
    \041\000\037\000\048\000\009\000\042\000\043\000\044\000\045\000\
    \046\000\047\000\061\000\085\000\048\000\038\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\039\000\039\000\049\000\
    \067\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\086\000\143\000\255\255\040\000\144\000\
    \145\000\146\000\055\000\148\000\055\000\149\000\048\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\255\255\048\000\150\000\
    \151\000\161\000\066\000\158\000\053\000\159\000\053\000\160\000\
    \051\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\165\000\
    \051\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\162\000\163\000\166\000\093\000\255\255\
    \002\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\255\255\077\000\103\000\108\000\116\000\
    \132\000\134\000\135\000\128\000\139\000\134\000\164\000\093\000\
    \171\000\077\000\167\000\168\000\169\000\172\000\112\000\173\000\
    \174\000\210\000\226\000\208\000\211\000\212\000\059\000\083\000\
    \134\000\213\000\214\000\215\000\216\000\218\000\141\000\219\000\
    \093\000\220\000\221\000\123\000\222\000\223\000\224\000\136\000\
    \095\000\225\000\035\001\065\001\234\000\155\000\005\001\097\001\
    \250\000\255\255\254\000\057\001\061\001\095\001\077\000\044\001\
    \092\001\088\001\009\001\029\001\076\000\124\000\033\001\018\001\
    \075\000\098\000\019\001\085\001\086\001\087\001\120\001\089\001\
    \074\000\225\000\053\001\121\001\073\000\090\001\072\000\071\000\
    \078\000\078\000\078\000\078\000\078\000\078\000\078\000\078\000\
    \078\000\078\000\098\000\113\001\122\000\091\001\064\000\004\001\
    \093\001\078\000\078\000\078\000\078\000\078\000\078\000\079\000\
    \079\000\079\000\079\000\079\000\079\000\079\000\079\000\079\000\
    \079\000\156\000\112\001\094\001\096\001\098\001\099\001\049\001\
    \079\000\079\000\079\000\079\000\079\000\079\000\100\001\157\000\
    \101\001\078\000\078\000\078\000\078\000\078\000\078\000\183\000\
    \184\000\184\000\184\000\184\000\184\000\184\000\184\000\184\000\
    \184\000\024\001\112\001\255\255\025\001\102\001\103\001\105\001\
    \079\000\079\000\079\000\079\000\079\000\079\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \106\001\107\001\048\001\040\001\108\001\109\001\110\001\080\000\
    \080\000\080\000\080\000\080\000\080\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\081\000\111\001\
    \027\001\255\255\171\001\031\001\170\001\023\001\081\000\081\000\
    \081\000\081\000\081\000\081\000\092\000\168\001\063\001\080\000\
    \080\000\080\000\080\000\080\000\080\000\248\000\165\001\252\000\
    \162\001\059\001\069\000\087\000\087\000\087\000\087\000\087\000\
    \087\000\087\000\087\000\087\000\087\000\255\255\081\000\081\000\
    \081\000\081\000\081\000\081\000\087\000\087\000\087\000\087\000\
    \087\000\087\000\088\000\088\000\088\000\088\000\088\000\088\000\
    \088\000\088\000\088\000\088\000\039\001\042\001\255\255\163\001\
    \164\001\120\000\002\001\088\000\088\000\088\000\088\000\088\000\
    \088\000\166\001\055\001\153\000\087\000\087\000\087\000\087\000\
    \087\000\087\000\007\001\167\001\164\001\169\001\016\001\164\001\
    \089\000\089\000\089\000\089\000\089\000\089\000\089\000\089\000\
    \089\000\089\000\051\001\088\000\088\000\088\000\088\000\088\000\
    \088\000\089\000\089\000\089\000\089\000\089\000\089\000\090\000\
    \090\000\090\000\090\000\090\000\090\000\090\000\090\000\090\000\
    \090\000\097\000\137\001\164\001\172\001\185\001\136\001\173\001\
    \090\000\090\000\090\000\090\000\090\000\090\000\097\000\179\000\
    \174\001\089\000\089\000\089\000\089\000\089\000\089\000\046\001\
    \175\001\176\001\180\000\164\001\184\001\181\000\182\000\182\000\
    \182\000\182\000\182\000\182\000\182\000\182\000\182\000\124\001\
    \090\000\090\000\090\000\090\000\090\000\090\000\192\001\178\001\
    \021\001\179\001\097\000\193\001\180\001\181\001\182\001\183\001\
    \164\001\216\001\255\255\097\000\184\001\216\001\209\001\097\000\
    \223\001\097\000\208\001\230\001\000\000\097\000\219\001\037\001\
    \216\001\217\001\000\000\220\001\216\001\097\000\000\000\000\000\
    \216\001\097\000\000\000\097\000\096\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\216\001\
    \000\000\126\001\000\000\000\000\000\000\000\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\097\000\000\000\218\001\250\001\
    \000\000\000\000\097\000\000\000\124\001\124\001\097\000\000\000\
    \221\001\000\000\253\001\000\000\000\000\000\000\097\000\255\255\
    \000\000\196\001\097\000\000\000\097\000\096\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\235\001\000\000\
    \241\001\000\000\255\001\242\001\000\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\101\000\101\000\101\000\101\000\101\000\
    \101\000\101\000\101\000\101\000\101\000\246\001\129\001\129\001\
    \228\001\000\000\196\001\000\000\101\000\101\000\101\000\101\000\
    \101\000\101\000\000\000\198\001\000\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\000\000\000\000\000\000\196\001\234\001\
    \134\001\097\000\097\000\097\000\097\000\097\000\097\000\097\000\
    \097\000\097\000\097\000\000\000\101\000\101\000\101\000\101\000\
    \101\000\101\000\097\000\097\000\097\000\097\000\097\000\097\000\
    \182\000\182\000\182\000\182\000\182\000\182\000\182\000\182\000\
    \182\000\182\000\000\000\000\000\201\001\177\000\184\000\184\000\
    \184\000\184\000\184\000\184\000\184\000\184\000\184\000\184\000\
    \188\000\000\000\097\000\097\000\097\000\097\000\097\000\097\000\
    \201\001\227\001\000\000\191\000\206\001\123\001\189\000\190\000\
    \190\000\190\000\190\000\190\000\190\000\190\000\190\000\190\000\
    \190\000\190\000\190\000\190\000\190\000\190\000\190\000\190\000\
    \190\000\190\000\189\000\190\000\190\000\190\000\190\000\190\000\
    \190\000\190\000\190\000\190\000\195\000\197\000\197\000\197\000\
    \197\000\197\000\197\000\197\000\197\000\197\000\197\000\198\000\
    \255\255\248\001\196\000\197\000\197\000\197\000\197\000\197\000\
    \197\000\197\000\197\000\197\000\196\000\197\000\197\000\197\000\
    \197\000\197\000\197\000\197\000\197\000\197\000\202\000\227\000\
    \227\000\227\000\227\000\227\000\227\000\227\000\227\000\227\000\
    \227\000\205\000\255\255\255\255\203\000\204\000\204\000\204\000\
    \204\000\204\000\204\000\204\000\204\000\204\000\226\000\195\001\
    \204\000\204\000\204\000\204\000\204\000\204\000\204\000\204\000\
    \204\000\204\000\232\001\000\000\000\000\206\000\221\001\239\001\
    \254\001\000\000\207\000\244\001\000\000\225\000\203\000\204\000\
    \204\000\204\000\204\000\204\000\204\000\204\000\204\000\204\000\
    \232\000\000\000\232\000\000\000\225\001\231\000\231\000\231\000\
    \231\000\231\000\231\000\231\000\231\000\231\000\231\000\217\000\
    \255\255\000\000\000\000\000\000\000\000\225\000\227\000\227\000\
    \227\000\227\000\227\000\227\000\227\000\227\000\227\000\227\000\
    \000\000\000\000\000\000\000\000\255\255\000\000\000\000\230\000\
    \000\000\230\000\000\000\228\000\229\000\229\000\229\000\229\000\
    \229\000\229\000\229\000\229\000\229\000\229\000\229\000\229\000\
    \229\000\229\000\229\000\229\000\229\000\229\000\229\000\229\000\
    \229\000\229\000\229\000\229\000\229\000\229\000\229\000\229\000\
    \229\000\229\000\000\000\228\000\231\000\231\000\231\000\231\000\
    \231\000\231\000\231\000\231\000\231\000\231\000\186\000\231\000\
    \231\000\231\000\231\000\231\000\231\000\231\000\231\000\231\000\
    \231\000\000\000\000\000\000\000\000\000\000\000\241\000\000\000\
    \113\001\000\000\077\001\077\001\077\001\077\001\077\001\077\001\
    \077\001\077\001\077\001\077\001\114\001\114\001\114\001\114\001\
    \114\001\114\001\114\001\114\001\114\001\114\001\000\000\112\001\
    \000\000\000\000\193\000\000\000\000\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\112\001\
    \000\000\000\000\000\000\240\000\200\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\000\000\246\000\000\000\000\000\240\000\000\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\000\000\000\000\000\000\000\000\
    \245\000\000\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\238\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \000\000\000\000\000\000\000\000\245\000\000\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \069\001\070\001\000\000\000\000\069\001\076\001\077\001\077\001\
    \077\001\077\001\077\001\077\001\077\001\077\001\077\001\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\069\001\
    \000\000\078\001\000\000\000\000\000\000\000\000\104\001\073\001\
    \000\000\000\000\000\000\000\000\079\001\000\000\071\001\076\001\
    \077\001\077\001\077\001\077\001\077\001\077\001\077\001\077\001\
    \077\001\000\000\000\000\072\001\000\000\000\000\000\000\000\000\
    \000\000\243\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \080\001\119\001\000\000\119\001\000\000\081\001\118\001\118\001\
    \118\001\118\001\118\001\118\001\118\001\118\001\118\001\118\001\
    \000\000\000\000\074\001\114\001\114\001\114\001\114\001\114\001\
    \114\001\114\001\114\001\114\001\114\001\083\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\082\001\000\000\000\000\
    \115\001\000\000\000\000\084\001\000\000\000\000\117\001\000\000\
    \117\001\000\000\075\001\116\001\116\001\116\001\116\001\116\001\
    \116\001\116\001\116\001\116\001\116\001\116\001\116\001\116\001\
    \116\001\116\001\116\001\116\001\116\001\116\001\116\001\000\000\
    \115\001\116\001\116\001\116\001\116\001\116\001\116\001\116\001\
    \116\001\116\001\116\001\118\001\118\001\118\001\118\001\118\001\
    \118\001\118\001\118\001\118\001\118\001\118\001\118\001\118\001\
    \118\001\118\001\118\001\118\001\118\001\118\001\118\001\000\000\
    \128\001\130\001\130\001\130\001\130\001\130\001\130\001\130\001\
    \130\001\130\001\130\001\000\000\000\000\128\001\000\000\000\000\
    \000\000\128\001\130\001\130\001\130\001\130\001\130\001\130\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\128\001\000\000\
    \000\000\185\001\000\000\155\001\155\001\155\001\155\001\155\001\
    \155\001\155\001\155\001\155\001\155\001\000\000\000\000\000\000\
    \000\000\000\000\130\001\130\001\130\001\130\001\130\001\130\001\
    \184\001\000\000\128\001\000\000\000\000\000\000\000\000\000\000\
    \128\001\000\000\000\000\000\000\128\001\000\000\000\000\000\000\
    \000\000\000\000\000\000\128\001\128\001\000\000\000\000\068\001\
    \128\001\128\001\128\001\127\001\000\000\128\001\000\000\000\000\
    \184\001\000\000\000\000\000\000\000\000\128\001\000\000\000\000\
    \000\000\128\001\000\000\128\001\127\001\131\001\131\001\131\001\
    \131\001\131\001\131\001\131\001\131\001\131\001\131\001\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\131\001\131\001\
    \131\001\131\001\131\001\131\001\132\001\132\001\132\001\132\001\
    \132\001\132\001\132\001\132\001\132\001\132\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\132\001\132\001\132\001\
    \132\001\132\001\132\001\000\000\000\000\000\000\131\001\131\001\
    \131\001\131\001\131\001\131\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\128\001\128\001\128\001\128\001\128\001\128\001\
    \128\001\128\001\128\001\128\001\000\000\132\001\132\001\132\001\
    \132\001\132\001\132\001\128\001\128\001\128\001\128\001\128\001\
    \128\001\191\001\142\001\191\001\000\000\000\000\190\001\190\001\
    \190\001\190\001\190\001\190\001\190\001\190\001\190\001\190\001\
    \186\001\186\001\186\001\186\001\186\001\186\001\186\001\186\001\
    \186\001\186\001\000\000\128\001\128\001\128\001\128\001\128\001\
    \128\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\000\000\000\000\000\000\000\000\141\001\
    \000\000\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\000\000\000\000\
    \000\000\000\000\141\001\000\000\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\146\001\147\001\
    \000\000\000\000\146\001\154\001\155\001\155\001\155\001\155\001\
    \155\001\155\001\155\001\155\001\155\001\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\200\001\146\001\000\000\153\001\
    \000\000\000\000\000\000\000\000\177\001\150\001\000\000\000\000\
    \000\000\200\001\156\001\000\000\148\001\154\001\155\001\155\001\
    \155\001\155\001\155\001\155\001\155\001\155\001\155\001\000\000\
    \000\000\149\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \139\001\000\000\000\000\000\000\000\000\000\000\157\001\000\000\
    \000\000\000\000\000\000\158\001\186\001\186\001\186\001\186\001\
    \186\001\186\001\186\001\186\001\186\001\186\001\200\001\000\000\
    \151\001\000\000\000\000\000\000\200\001\000\000\000\000\000\000\
    \200\001\187\001\000\000\160\001\000\000\000\000\000\000\000\000\
    \200\001\000\000\000\000\159\001\200\001\000\000\200\001\199\001\
    \000\000\161\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \152\001\000\000\000\000\000\000\000\000\189\001\000\000\189\001\
    \000\000\187\001\188\001\188\001\188\001\188\001\188\001\188\001\
    \188\001\188\001\188\001\188\001\188\001\188\001\188\001\188\001\
    \188\001\188\001\188\001\188\001\188\001\188\001\188\001\188\001\
    \188\001\188\001\188\001\188\001\188\001\188\001\188\001\188\001\
    \190\001\190\001\190\001\190\001\190\001\190\001\190\001\190\001\
    \190\001\190\001\190\001\190\001\190\001\190\001\190\001\190\001\
    \190\001\190\001\190\001\190\001\202\001\202\001\202\001\202\001\
    \202\001\202\001\202\001\202\001\202\001\202\001\200\001\000\000\
    \000\000\000\000\000\000\000\000\000\000\202\001\202\001\202\001\
    \202\001\202\001\202\001\200\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\202\001\202\001\202\001\
    \202\001\202\001\202\001\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \200\001\000\000\000\000\000\000\000\000\145\001\200\001\000\000\
    \000\000\000\000\200\001\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\200\001\000\000\000\000\000\000\200\001\000\000\
    \200\001\199\001\203\001\203\001\203\001\203\001\203\001\203\001\
    \203\001\203\001\203\001\203\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\203\001\203\001\203\001\203\001\203\001\
    \203\001\204\001\204\001\204\001\204\001\204\001\204\001\204\001\
    \204\001\204\001\204\001\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\204\001\204\001\204\001\204\001\204\001\204\001\
    \000\000\000\000\000\000\203\001\203\001\203\001\203\001\203\001\
    \203\001\000\000\000\000\000\000\000\000\000\000\000\000\200\001\
    \200\001\200\001\200\001\200\001\200\001\200\001\200\001\200\001\
    \200\001\000\000\204\001\204\001\204\001\204\001\204\001\204\001\
    \200\001\200\001\200\001\200\001\200\001\200\001\000\000\214\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \200\001\200\001\200\001\200\001\200\001\200\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \000\000\000\000\000\000\000\000\213\001\000\000\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\000\000\000\000\000\000\000\000\213\001\
    \000\000\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\211\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\255\255\003\000\000\000\134\000\255\255\
    \003\000\255\255\134\000\069\001\146\001\057\000\255\255\069\001\
    \146\001\255\255\255\255\255\255\255\255\125\000\138\000\255\255\
    \000\000\255\255\000\000\003\000\169\000\134\000\174\000\255\255\
    \000\000\010\001\069\001\146\001\012\001\000\000\010\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\005\000\115\000\000\000\125\000\129\000\005\000\
    \236\001\136\000\255\001\038\000\255\255\010\000\136\000\102\000\
    \058\000\000\000\107\000\102\000\255\255\011\000\000\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\029\000\038\000\000\000\111\000\208\000\233\000\255\000\
    \012\001\015\000\017\000\060\000\011\000\010\000\000\000\020\000\
    \024\000\031\000\032\000\034\000\022\000\026\000\000\000\014\000\
    \027\000\033\000\018\000\023\000\000\000\016\000\019\000\035\000\
    \040\000\036\000\038\000\000\000\041\000\042\000\043\000\044\000\
    \045\000\046\000\058\000\082\000\011\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\039\000\
    \063\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\085\000\140\000\060\000\013\000\143\000\
    \144\000\145\000\048\000\147\000\048\000\148\000\039\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\049\000\049\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\065\000\039\000\149\000\
    \150\000\156\000\063\000\157\000\051\000\158\000\051\000\159\000\
    \050\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\155\000\
    \050\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\161\000\162\000\155\000\091\000\065\000\
    \000\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\057\000\068\000\102\000\107\000\115\000\
    \131\000\133\000\133\000\125\000\138\000\133\000\163\000\094\000\
    \165\000\068\000\166\000\167\000\168\000\171\000\111\000\172\000\
    \173\000\206\000\203\000\207\000\210\000\211\000\058\000\082\000\
    \133\000\212\000\213\000\214\000\215\000\217\000\140\000\218\000\
    \097\000\219\000\220\000\119\000\221\000\222\000\223\000\133\000\
    \091\000\203\000\034\001\062\001\233\000\152\000\001\001\080\001\
    \247\000\060\000\251\000\054\001\058\001\081\001\068\000\041\001\
    \082\001\083\001\006\001\026\001\068\000\119\000\030\001\015\001\
    \068\000\094\000\015\001\084\001\085\001\086\001\071\001\088\001\
    \068\000\203\000\050\001\071\001\068\000\089\001\068\000\068\000\
    \071\000\071\000\071\000\071\000\071\000\071\000\071\000\071\000\
    \071\000\071\000\097\000\076\001\119\000\090\001\063\000\001\001\
    \092\001\071\000\071\000\071\000\071\000\071\000\071\000\078\000\
    \078\000\078\000\078\000\078\000\078\000\078\000\078\000\078\000\
    \078\000\152\000\076\001\093\001\095\001\097\001\098\001\045\001\
    \078\000\078\000\078\000\078\000\078\000\078\000\099\001\152\000\
    \100\001\071\000\071\000\071\000\071\000\071\000\071\000\180\000\
    \180\000\180\000\180\000\180\000\180\000\180\000\180\000\180\000\
    \180\000\020\001\076\001\065\000\020\001\101\001\102\001\104\001\
    \078\000\078\000\078\000\078\000\078\000\078\000\079\000\079\000\
    \079\000\079\000\079\000\079\000\079\000\079\000\079\000\079\000\
    \105\001\106\001\045\001\036\001\107\001\108\001\109\001\079\000\
    \079\000\079\000\079\000\079\000\079\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\110\001\
    \026\001\121\001\157\001\030\001\158\001\020\001\080\000\080\000\
    \080\000\080\000\080\000\080\000\091\000\159\001\062\001\079\000\
    \079\000\079\000\079\000\079\000\079\000\247\000\160\001\251\000\
    \161\001\058\001\068\000\086\000\086\000\086\000\086\000\086\000\
    \086\000\086\000\086\000\086\000\086\000\094\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\086\000\086\000\086\000\086\000\
    \086\000\086\000\087\000\087\000\087\000\087\000\087\000\087\000\
    \087\000\087\000\087\000\087\000\036\001\041\001\097\000\162\001\
    \163\001\119\000\001\001\087\000\087\000\087\000\087\000\087\000\
    \087\000\165\001\054\001\152\000\086\000\086\000\086\000\086\000\
    \086\000\086\000\006\001\166\001\167\001\168\001\015\001\169\001\
    \088\000\088\000\088\000\088\000\088\000\088\000\088\000\088\000\
    \088\000\088\000\050\001\087\000\087\000\087\000\087\000\087\000\
    \087\000\088\000\088\000\088\000\088\000\088\000\088\000\089\000\
    \089\000\089\000\089\000\089\000\089\000\089\000\089\000\089\000\
    \089\000\095\000\133\001\170\001\171\001\154\001\133\001\172\001\
    \089\000\089\000\089\000\089\000\089\000\089\000\095\000\176\000\
    \173\001\088\000\088\000\088\000\088\000\088\000\088\000\045\001\
    \174\001\175\001\176\000\176\001\154\001\176\000\176\000\176\000\
    \176\000\176\000\176\000\176\000\176\000\176\000\176\000\122\001\
    \089\000\089\000\089\000\089\000\089\000\089\000\148\001\177\001\
    \020\001\178\001\098\000\148\001\179\001\180\001\181\001\182\001\
    \183\001\216\001\193\001\095\000\154\001\216\001\205\001\098\000\
    \222\001\095\000\205\001\229\001\255\255\095\000\218\001\036\001\
    \215\001\215\001\255\255\218\001\215\001\095\000\255\255\255\255\
    \216\001\095\000\255\255\095\000\095\000\096\000\096\000\096\000\
    \096\000\096\000\096\000\096\000\096\000\096\000\096\000\215\001\
    \255\255\122\001\255\255\255\255\255\255\255\255\096\000\096\000\
    \096\000\096\000\096\000\096\000\098\000\255\255\215\001\247\001\
    \255\255\255\255\098\000\255\255\125\001\128\001\098\000\255\255\
    \220\001\255\255\251\001\255\255\255\255\255\255\098\000\121\001\
    \255\255\194\001\098\000\255\255\098\000\098\000\096\000\096\000\
    \096\000\096\000\096\000\096\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\231\001\255\255\
    \238\001\255\255\251\001\238\001\255\255\099\000\099\000\099\000\
    \099\000\099\000\099\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\243\001\125\001\128\001\
    \224\001\255\255\197\001\255\255\100\000\100\000\100\000\100\000\
    \100\000\100\000\255\255\194\001\255\255\099\000\099\000\099\000\
    \099\000\099\000\099\000\255\255\255\255\255\255\200\001\231\001\
    \133\001\101\000\101\000\101\000\101\000\101\000\101\000\101\000\
    \101\000\101\000\101\000\255\255\100\000\100\000\100\000\100\000\
    \100\000\100\000\101\000\101\000\101\000\101\000\101\000\101\000\
    \182\000\182\000\182\000\182\000\182\000\182\000\182\000\182\000\
    \182\000\182\000\255\255\255\255\197\001\176\000\184\000\184\000\
    \184\000\184\000\184\000\184\000\184\000\184\000\184\000\184\000\
    \185\000\255\255\101\000\101\000\101\000\101\000\101\000\101\000\
    \200\001\224\001\255\255\185\000\205\001\122\001\185\000\185\000\
    \185\000\185\000\185\000\185\000\185\000\185\000\185\000\185\000\
    \190\000\190\000\190\000\190\000\190\000\190\000\190\000\190\000\
    \190\000\190\000\191\000\191\000\191\000\191\000\191\000\191\000\
    \191\000\191\000\191\000\191\000\192\000\197\000\197\000\197\000\
    \197\000\197\000\197\000\197\000\197\000\197\000\197\000\192\000\
    \193\001\247\001\192\000\192\000\192\000\192\000\192\000\192\000\
    \192\000\192\000\192\000\192\000\198\000\198\000\198\000\198\000\
    \198\000\198\000\198\000\198\000\198\000\198\000\199\000\226\000\
    \226\000\226\000\226\000\226\000\226\000\226\000\226\000\226\000\
    \226\000\199\000\125\001\128\001\199\000\199\000\199\000\199\000\
    \199\000\199\000\199\000\199\000\199\000\199\000\204\000\194\001\
    \204\000\204\000\204\000\204\000\204\000\204\000\204\000\204\000\
    \204\000\204\000\231\001\255\255\255\255\199\000\220\001\238\001\
    \251\001\255\255\199\000\243\001\255\255\204\000\205\000\205\000\
    \205\000\205\000\205\000\205\000\205\000\205\000\205\000\205\000\
    \225\000\255\255\225\000\255\255\224\001\225\000\225\000\225\000\
    \225\000\225\000\225\000\225\000\225\000\225\000\225\000\205\000\
    \197\001\255\255\255\255\255\255\255\255\204\000\227\000\227\000\
    \227\000\227\000\227\000\227\000\227\000\227\000\227\000\227\000\
    \255\255\255\255\255\255\255\255\200\001\255\255\255\255\228\000\
    \255\255\228\000\255\255\227\000\228\000\228\000\228\000\228\000\
    \228\000\228\000\228\000\228\000\228\000\228\000\229\000\229\000\
    \229\000\229\000\229\000\229\000\229\000\229\000\229\000\229\000\
    \230\000\230\000\230\000\230\000\230\000\230\000\230\000\230\000\
    \230\000\230\000\255\255\227\000\231\000\231\000\231\000\231\000\
    \231\000\231\000\231\000\231\000\231\000\231\000\185\000\232\000\
    \232\000\232\000\232\000\232\000\232\000\232\000\232\000\232\000\
    \232\000\255\255\255\255\255\255\255\255\255\255\237\000\255\255\
    \077\001\255\255\077\001\077\001\077\001\077\001\077\001\077\001\
    \077\001\077\001\077\001\077\001\113\001\113\001\113\001\113\001\
    \113\001\113\001\113\001\113\001\113\001\113\001\255\255\077\001\
    \255\255\255\255\192\000\255\255\255\255\237\000\237\000\237\000\
    \237\000\237\000\237\000\237\000\237\000\237\000\237\000\237\000\
    \237\000\237\000\237\000\237\000\237\000\237\000\237\000\237\000\
    \237\000\237\000\237\000\237\000\237\000\237\000\237\000\077\001\
    \255\255\255\255\255\255\237\000\199\000\237\000\237\000\237\000\
    \237\000\237\000\237\000\237\000\237\000\237\000\237\000\237\000\
    \237\000\237\000\237\000\237\000\237\000\237\000\237\000\237\000\
    \237\000\237\000\237\000\237\000\237\000\237\000\237\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\255\255\242\000\255\255\255\255\240\000\255\255\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\240\000\240\000\240\000\240\000\240\000\240\000\
    \240\000\240\000\242\000\242\000\242\000\242\000\242\000\242\000\
    \242\000\242\000\242\000\242\000\242\000\242\000\242\000\242\000\
    \242\000\242\000\242\000\242\000\242\000\242\000\242\000\242\000\
    \242\000\242\000\242\000\242\000\255\255\255\255\255\255\255\255\
    \242\000\255\255\242\000\242\000\242\000\242\000\242\000\242\000\
    \242\000\242\000\242\000\242\000\242\000\242\000\242\000\242\000\
    \242\000\242\000\242\000\242\000\242\000\242\000\242\000\242\000\
    \242\000\242\000\242\000\242\000\237\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \255\255\255\255\255\255\255\255\245\000\255\255\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \245\000\245\000\245\000\245\000\245\000\245\000\245\000\245\000\
    \066\001\066\001\255\255\255\255\066\001\079\001\079\001\079\001\
    \079\001\079\001\079\001\079\001\079\001\079\001\079\001\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\066\001\
    \255\255\066\001\255\255\255\255\255\255\255\255\079\001\066\001\
    \255\255\255\255\255\255\255\255\066\001\255\255\066\001\066\001\
    \066\001\066\001\066\001\066\001\066\001\066\001\066\001\066\001\
    \066\001\255\255\255\255\066\001\255\255\255\255\255\255\255\255\
    \255\255\242\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \066\001\112\001\255\255\112\001\255\255\066\001\112\001\112\001\
    \112\001\112\001\112\001\112\001\112\001\112\001\112\001\112\001\
    \255\255\255\255\066\001\114\001\114\001\114\001\114\001\114\001\
    \114\001\114\001\114\001\114\001\114\001\066\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\066\001\255\255\255\255\
    \114\001\255\255\255\255\066\001\255\255\255\255\115\001\255\255\
    \115\001\255\255\066\001\115\001\115\001\115\001\115\001\115\001\
    \115\001\115\001\115\001\115\001\115\001\116\001\116\001\116\001\
    \116\001\116\001\116\001\116\001\116\001\116\001\116\001\255\255\
    \114\001\117\001\117\001\117\001\117\001\117\001\117\001\117\001\
    \117\001\117\001\117\001\118\001\118\001\118\001\118\001\118\001\
    \118\001\118\001\118\001\118\001\118\001\119\001\119\001\119\001\
    \119\001\119\001\119\001\119\001\119\001\119\001\119\001\255\255\
    \126\001\127\001\127\001\127\001\127\001\127\001\127\001\127\001\
    \127\001\127\001\127\001\255\255\255\255\126\001\255\255\255\255\
    \255\255\129\001\127\001\127\001\127\001\127\001\127\001\127\001\
    \255\255\255\255\255\255\255\255\255\255\255\255\129\001\255\255\
    \255\255\155\001\255\255\155\001\155\001\155\001\155\001\155\001\
    \155\001\155\001\155\001\155\001\155\001\255\255\255\255\255\255\
    \255\255\255\255\127\001\127\001\127\001\127\001\127\001\127\001\
    \155\001\255\255\126\001\255\255\255\255\255\255\255\255\255\255\
    \126\001\255\255\255\255\255\255\126\001\255\255\255\255\255\255\
    \255\255\255\255\255\255\129\001\126\001\255\255\255\255\066\001\
    \126\001\129\001\126\001\126\001\255\255\129\001\255\255\255\255\
    \155\001\255\255\255\255\255\255\255\255\129\001\255\255\255\255\
    \255\255\129\001\255\255\129\001\129\001\130\001\130\001\130\001\
    \130\001\130\001\130\001\130\001\130\001\130\001\130\001\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\130\001\130\001\
    \130\001\130\001\130\001\130\001\131\001\131\001\131\001\131\001\
    \131\001\131\001\131\001\131\001\131\001\131\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\131\001\131\001\131\001\
    \131\001\131\001\131\001\255\255\255\255\255\255\130\001\130\001\
    \130\001\130\001\130\001\130\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\132\001\132\001\132\001\132\001\132\001\132\001\
    \132\001\132\001\132\001\132\001\255\255\131\001\131\001\131\001\
    \131\001\131\001\131\001\132\001\132\001\132\001\132\001\132\001\
    \132\001\184\001\138\001\184\001\255\255\255\255\184\001\184\001\
    \184\001\184\001\184\001\184\001\184\001\184\001\184\001\184\001\
    \185\001\185\001\185\001\185\001\185\001\185\001\185\001\185\001\
    \185\001\185\001\255\255\132\001\132\001\132\001\132\001\132\001\
    \132\001\138\001\138\001\138\001\138\001\138\001\138\001\138\001\
    \138\001\138\001\138\001\138\001\138\001\138\001\138\001\138\001\
    \138\001\138\001\138\001\138\001\138\001\138\001\138\001\138\001\
    \138\001\138\001\138\001\255\255\255\255\255\255\255\255\138\001\
    \255\255\138\001\138\001\138\001\138\001\138\001\138\001\138\001\
    \138\001\138\001\138\001\138\001\138\001\138\001\138\001\138\001\
    \138\001\138\001\138\001\138\001\138\001\138\001\138\001\138\001\
    \138\001\138\001\138\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\255\255\255\255\
    \255\255\255\255\141\001\255\255\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\141\001\141\001\
    \141\001\141\001\141\001\141\001\141\001\141\001\143\001\143\001\
    \255\255\255\255\143\001\156\001\156\001\156\001\156\001\156\001\
    \156\001\156\001\156\001\156\001\156\001\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\198\001\143\001\255\255\143\001\
    \255\255\255\255\255\255\255\255\156\001\143\001\255\255\255\255\
    \255\255\198\001\143\001\255\255\143\001\143\001\143\001\143\001\
    \143\001\143\001\143\001\143\001\143\001\143\001\143\001\255\255\
    \255\255\143\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \138\001\255\255\255\255\255\255\255\255\255\255\143\001\255\255\
    \255\255\255\255\255\255\143\001\186\001\186\001\186\001\186\001\
    \186\001\186\001\186\001\186\001\186\001\186\001\198\001\255\255\
    \143\001\255\255\255\255\255\255\198\001\255\255\255\255\255\255\
    \198\001\186\001\255\255\143\001\255\255\255\255\255\255\255\255\
    \198\001\255\255\255\255\143\001\198\001\255\255\198\001\198\001\
    \255\255\143\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \143\001\255\255\255\255\255\255\255\255\187\001\255\255\187\001\
    \255\255\186\001\187\001\187\001\187\001\187\001\187\001\187\001\
    \187\001\187\001\187\001\187\001\188\001\188\001\188\001\188\001\
    \188\001\188\001\188\001\188\001\188\001\188\001\189\001\189\001\
    \189\001\189\001\189\001\189\001\189\001\189\001\189\001\189\001\
    \190\001\190\001\190\001\190\001\190\001\190\001\190\001\190\001\
    \190\001\190\001\191\001\191\001\191\001\191\001\191\001\191\001\
    \191\001\191\001\191\001\191\001\199\001\199\001\199\001\199\001\
    \199\001\199\001\199\001\199\001\199\001\199\001\201\001\255\255\
    \255\255\255\255\255\255\255\255\255\255\199\001\199\001\199\001\
    \199\001\199\001\199\001\201\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\199\001\199\001\199\001\
    \199\001\199\001\199\001\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \201\001\255\255\255\255\255\255\255\255\143\001\201\001\255\255\
    \255\255\255\255\201\001\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\201\001\255\255\255\255\255\255\201\001\255\255\
    \201\001\201\001\202\001\202\001\202\001\202\001\202\001\202\001\
    \202\001\202\001\202\001\202\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\202\001\202\001\202\001\202\001\202\001\
    \202\001\203\001\203\001\203\001\203\001\203\001\203\001\203\001\
    \203\001\203\001\203\001\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\203\001\203\001\203\001\203\001\203\001\203\001\
    \255\255\255\255\255\255\202\001\202\001\202\001\202\001\202\001\
    \202\001\255\255\255\255\255\255\255\255\255\255\255\255\204\001\
    \204\001\204\001\204\001\204\001\204\001\204\001\204\001\204\001\
    \204\001\255\255\203\001\203\001\203\001\203\001\203\001\203\001\
    \204\001\204\001\204\001\204\001\204\001\204\001\255\255\210\001\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \204\001\204\001\204\001\204\001\204\001\204\001\210\001\210\001\
    \210\001\210\001\210\001\210\001\210\001\210\001\210\001\210\001\
    \210\001\210\001\210\001\210\001\210\001\210\001\210\001\210\001\
    \210\001\210\001\210\001\210\001\210\001\210\001\210\001\210\001\
    \255\255\255\255\255\255\255\255\210\001\255\255\210\001\210\001\
    \210\001\210\001\210\001\210\001\210\001\210\001\210\001\210\001\
    \210\001\210\001\210\001\210\001\210\001\210\001\210\001\210\001\
    \210\001\210\001\210\001\210\001\210\001\210\001\210\001\210\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\255\255\255\255\255\255\255\255\213\001\
    \255\255\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\213\001\213\001\213\001\213\001\213\001\
    \213\001\213\001\213\001\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\210\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec read_json v lexbuf =
   __ocaml_lex_read_json_rec v lexbuf 0
and __ocaml_lex_read_json_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 174 "lib/read.mll"
                
# 174 "lib/read.mll"
                ( `Bool true )

# 1016 "lib/read.ml"

  
# 1017 "lib/read.ml"
  | 1 ->

# 175 "lib/read.mll"
                
# 175 "lib/read.mll"
                ( `Bool false )

# 1021 "lib/read.ml"

  
# 1022 "lib/read.ml"
  | 2 ->

# 176 "lib/read.mll"
                
# 176 "lib/read.mll"
                ( `Null )

# 1026 "lib/read.ml"

  
# 1027 "lib/read.ml"
  | 3 ->

# 177 "lib/read.mll"
                
# 177 "lib/read.mll"
                (
                    
# 179 "lib/read.mll"
                    `Float nan
                
# 183 "lib/read.mll"
                )

# 1037 "lib/read.ml"

  
# 1038 "lib/read.ml"
  | 4 ->

# 184 "lib/read.mll"
                
# 184 "lib/read.mll"
                (
                    
# 186 "lib/read.mll"
                    `Float infinity
                
# 190 "lib/read.mll"
                )

# 1048 "lib/read.ml"

  
# 1049 "lib/read.ml"
  | 5 ->

# 191 "lib/read.mll"
                
# 191 "lib/read.mll"
                (
                    
# 193 "lib/read.mll"
                    `Float neg_infinity
                
# 197 "lib/read.mll"
                )

# 1059 "lib/read.ml"

  
# 1060 "lib/read.ml"
  | 6 ->

# 198 "lib/read.mll"
                
# 198 "lib/read.mll"
                (
                    
# 200 "lib/read.mll"
                    Buffer.clear v.buf;
                    `String (finish_string v lexbuf)
                
# 205 "lib/read.mll"
                )

# 1071 "lib/read.ml"

  
# 1072 "lib/read.ml"
  | 7 ->

# 206 "lib/read.mll"
                         
# 206 "lib/read.mll"
                         ( make_positive_int v lexbuf )

# 1076 "lib/read.ml"

  
# 1077 "lib/read.ml"
  | 8 ->

# 207 "lib/read.mll"
                         
# 207 "lib/read.mll"
                         ( make_negative_int v lexbuf )

# 1081 "lib/read.ml"

  
# 1082 "lib/read.ml"
  | 9 ->

# 208 "lib/read.mll"
                
# 208 "lib/read.mll"
                (
                    
# 210 "lib/read.mll"
                    `Float (float_of_string (lexeme lexbuf))
                 
# 214 "lib/read.mll"
                 )

# 1092 "lib/read.ml"

  
# 1093 "lib/read.ml"
  | 10 ->

# 216 "lib/read.mll"
                 
# 216 "lib/read.mll"
                 ( let acc = ref [] in
                   try
                     read_space v lexbuf;
                     read_object_end lexbuf;
                     let field_name = read_ident v lexbuf in
                     read_space v lexbuf;
                     read_colon v lexbuf;
                     read_space v lexbuf;
                     acc := (field_name, read_json v lexbuf) :: !acc;
                     while true do
                       read_space v lexbuf;
                       read_object_sep v lexbuf;
                       read_space v lexbuf;
                       let field_name = read_ident v lexbuf in
                       read_space v lexbuf;
                       read_colon v lexbuf;
                       read_space v lexbuf;
                       acc := (field_name, read_json v lexbuf) :: !acc;
                     done;
                     assert false
                   with Common.End_of_object ->
                     `Assoc (List.rev !acc)
                 )

# 1119 "lib/read.ml"

  
# 1120 "lib/read.ml"
  | 11 ->

# 240 "lib/read.mll"
                 
# 240 "lib/read.mll"
                 ( let acc = ref [] in
                   try
                     read_space v lexbuf;
                     read_array_end lexbuf;
                     acc := read_json v lexbuf :: !acc;
                     while true do
                       read_space v lexbuf;
                       read_array_sep v lexbuf;
                       read_space v lexbuf;
                       acc := read_json v lexbuf :: !acc;
                     done;
                     assert false
                   with Common.End_of_array ->
                     `List (List.rev !acc)
                 )

# 1138 "lib/read.ml"

  
# 1139 "lib/read.ml"
  | 12 ->

# 256 "lib/read.mll"
                 
# 256 "lib/read.mll"
                 (
                     
# 273 "lib/read.mll"
                     long_error "Invalid token" v lexbuf
                 
# 275 "lib/read.mll"
                 )

# 1162 "lib/read.ml"

  
# 1163 "lib/read.ml"
  | 13 ->

# 277 "lib/read.mll"
                 
# 277 "lib/read.mll"
                 (
                     
# 284 "lib/read.mll"
                     long_error "Invalid token" v lexbuf
                 
# 286 "lib/read.mll"
                 )

# 1176 "lib/read.ml"

  
# 1177 "lib/read.ml"
  | 14 ->

# 288 "lib/read.mll"
                 
# 288 "lib/read.mll"
                 ( read_json v lexbuf )

# 1181 "lib/read.ml"

  
# 1182 "lib/read.ml"
  | 15 ->

# 289 "lib/read.mll"
                 
# 289 "lib/read.mll"
                 ( finish_comment v lexbuf; read_json v lexbuf )

# 1186 "lib/read.ml"

  
# 1187 "lib/read.ml"
  | 16 ->

# 290 "lib/read.mll"
                 
# 290 "lib/read.mll"
                 ( newline v lexbuf; read_json v lexbuf )

# 1191 "lib/read.ml"

  
# 1192 "lib/read.ml"
  | 17 ->

# 291 "lib/read.mll"
                 
# 291 "lib/read.mll"
                 ( read_json v lexbuf )

# 1196 "lib/read.ml"

  
# 1197 "lib/read.ml"
  | 18 ->

# 292 "lib/read.mll"
                 
# 292 "lib/read.mll"
                 ( custom_error "Unexpected end of input" v lexbuf )

# 1201 "lib/read.ml"

  
# 1202 "lib/read.ml"
  | 19 ->

# 293 "lib/read.mll"
                 
# 293 "lib/read.mll"
                 ( long_error "Invalid token" v lexbuf )

# 1206 "lib/read.ml"

  
# 1207 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_json_rec v lexbuf __ocaml_lex_state

and finish_string v lexbuf =
   __ocaml_lex_finish_string_rec v lexbuf 58
and __ocaml_lex_finish_string_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 297 "lib/read.mll"
                  
# 297 "lib/read.mll"
                  ( Buffer.contents v.buf )

# 1218 "lib/read.ml"

  
# 1219 "lib/read.ml"
  | 1 ->

# 298 "lib/read.mll"
                  
# 298 "lib/read.mll"
                  ( finish_escaped_char v lexbuf;
                    finish_string v lexbuf )

# 1224 "lib/read.ml"

  
# 1225 "lib/read.ml"
  | 2 ->

# 300 "lib/read.mll"
                  
# 300 "lib/read.mll"
                  ( add_lexeme v.buf lexbuf;
                    finish_string v lexbuf )

# 1230 "lib/read.ml"

  
# 1231 "lib/read.ml"
  | 3 ->

# 302 "lib/read.mll"
                  
# 302 "lib/read.mll"
                  ( custom_error "Unexpected end of input" v lexbuf )

# 1235 "lib/read.ml"

  
# 1236 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_string_rec v lexbuf __ocaml_lex_state

and map_string v f lexbuf =
   __ocaml_lex_map_string_rec v f lexbuf 63
and __ocaml_lex_map_string_rec v f lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 305 "lib/read.mll"
                  
# 305 "lib/read.mll"
                  ( let b = v.buf in
                    f (Buffer.contents b) 0 (Buffer.length b) )

# 1248 "lib/read.ml"

  
# 1249 "lib/read.ml"
  | 1 ->

# 307 "lib/read.mll"
                  
# 307 "lib/read.mll"
                  ( finish_escaped_char v lexbuf;
                    map_string v f lexbuf )

# 1254 "lib/read.ml"

  
# 1255 "lib/read.ml"
  | 2 ->

# 309 "lib/read.mll"
                  
# 309 "lib/read.mll"
                  ( add_lexeme v.buf lexbuf;
                    map_string v f lexbuf )

# 1260 "lib/read.ml"

  
# 1261 "lib/read.ml"
  | 3 ->

# 311 "lib/read.mll"
                  
# 311 "lib/read.mll"
                  ( custom_error "Unexpected end of input" v lexbuf )

# 1265 "lib/read.ml"

  
# 1266 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_map_string_rec v f lexbuf __ocaml_lex_state

and finish_escaped_char v lexbuf =
   __ocaml_lex_finish_escaped_char_rec v lexbuf 68
and __ocaml_lex_finish_escaped_char_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let

# 316 "lib/read.mll"
           
# 316 "lib/read.mll"
           c

# 1278 "lib/read.ml"
# 1278 "lib/read.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in

# 316 "lib/read.mll"
             
# 316 "lib/read.mll"
             ( Buffer.add_char v.buf c )

# 1282 "lib/read.ml"

  
# 1283 "lib/read.ml"
  | 1 ->

# 317 "lib/read.mll"
         
# 317 "lib/read.mll"
         ( Buffer.add_char v.buf '\b' )

# 1287 "lib/read.ml"

  
# 1288 "lib/read.ml"
  | 2 ->

# 318 "lib/read.mll"
         
# 318 "lib/read.mll"
         ( Buffer.add_char v.buf '\012' )

# 1292 "lib/read.ml"

  
# 1293 "lib/read.ml"
  | 3 ->

# 319 "lib/read.mll"
         
# 319 "lib/read.mll"
         ( Buffer.add_char v.buf '\n' )

# 1297 "lib/read.ml"

  
# 1298 "lib/read.ml"
  | 4 ->

# 320 "lib/read.mll"
         
# 320 "lib/read.mll"
         ( Buffer.add_char v.buf '\r' )

# 1302 "lib/read.ml"

  
# 1303 "lib/read.ml"
  | 5 ->

# 321 "lib/read.mll"
         
# 321 "lib/read.mll"
         ( Buffer.add_char v.buf '\t' )

# 1307 "lib/read.ml"

  
# 1308 "lib/read.ml"
  | 6 ->
let

# 322 "lib/read.mll"
                
# 322 "lib/read.mll"
                a

# 1313 "lib/read.ml"
# 1313 "lib/read.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1)
and

# 322 "lib/read.mll"
                           
# 322 "lib/read.mll"
                           b

# 1318 "lib/read.ml"
# 1318 "lib/read.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 2)
and

# 322 "lib/read.mll"
                                      
# 322 "lib/read.mll"
                                      c

# 1323 "lib/read.ml"
# 1323 "lib/read.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 3)
and

# 322 "lib/read.mll"
                                                 
# 322 "lib/read.mll"
                                                 d

# 1328 "lib/read.ml"
# 1328 "lib/read.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 4) in

# 323 "lib/read.mll"
         
# 323 "lib/read.mll"
         ( let x =
             (hex a lsl 12) lor (hex b lsl 8) lor (hex c lsl 4) lor hex d
           in
           if x >= 0xD800 && x <= 0xDBFF then
             finish_surrogate_pair v x lexbuf
           else
             Codec.utf8_of_code v.buf x

         )

# 1340 "lib/read.ml"

  
# 1341 "lib/read.ml"
  | 7 ->

# 332 "lib/read.mll"
         
# 332 "lib/read.mll"
         ( long_error "Invalid escape sequence" v lexbuf )

# 1345 "lib/read.ml"

  
# 1346 "lib/read.ml"
  | 8 ->

# 333 "lib/read.mll"
         
# 333 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )

# 1350 "lib/read.ml"

  
# 1351 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_escaped_char_rec v lexbuf __ocaml_lex_state

and finish_surrogate_pair v x lexbuf =
   __ocaml_lex_finish_surrogate_pair_rec v x lexbuf 82
and __ocaml_lex_finish_surrogate_pair_rec v x lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let

# 336 "lib/read.mll"
                  
# 336 "lib/read.mll"
                  a

# 1363 "lib/read.ml"
# 1363 "lib/read.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 2)
and

# 336 "lib/read.mll"
                             
# 336 "lib/read.mll"
                             b

# 1368 "lib/read.ml"
# 1368 "lib/read.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 3)
and

# 336 "lib/read.mll"
                                        
# 336 "lib/read.mll"
                                        c

# 1373 "lib/read.ml"
# 1373 "lib/read.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 4)
and

# 336 "lib/read.mll"
                                                   
# 336 "lib/read.mll"
                                                   d

# 1378 "lib/read.ml"
# 1378 "lib/read.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 5) in

# 337 "lib/read.mll"
         
# 337 "lib/read.mll"
         ( let y =
             (hex a lsl 12) lor (hex b lsl 8) lor (hex c lsl 4) lor hex d
           in
           if y >= 0xDC00 && y <= 0xDFFF then
             Codec.utf8_of_surrogate_pair v.buf x y
           else
             long_error "Invalid low surrogate for code point beyond U+FFFF"
               v lexbuf
         )

# 1390 "lib/read.ml"

  
# 1391 "lib/read.ml"
  | 1 ->

# 346 "lib/read.mll"
         
# 346 "lib/read.mll"
         ( long_error "Missing escape sequence representing low surrogate \
                       for code point beyond U+FFFF" v lexbuf )

# 1396 "lib/read.ml"

  
# 1397 "lib/read.ml"
  | 2 ->

# 348 "lib/read.mll"
         
# 348 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )

# 1401 "lib/read.ml"

  
# 1402 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_surrogate_pair_rec v x lexbuf __ocaml_lex_state

and finish_stringlit v lexbuf =
   __ocaml_lex_finish_stringlit_rec v lexbuf 91
and __ocaml_lex_finish_stringlit_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 353 "lib/read.mll"
         
# 353 "lib/read.mll"
         ( let len = lexbuf.lex_curr_pos - lexbuf.lex_start_pos in
           let s = Bytes.create (len+1) in
           Bytes.set s 0 '"';
           Bytes.blit lexbuf.lex_buffer lexbuf.lex_start_pos s 1 len;
           Bytes.to_string s
         )

# 1418 "lib/read.ml"

  
# 1419 "lib/read.ml"
  | 1 ->

# 359 "lib/read.mll"
         
# 359 "lib/read.mll"
         ( long_error "Invalid string literal" v lexbuf )

# 1423 "lib/read.ml"

  
# 1424 "lib/read.ml"
  | 2 ->

# 360 "lib/read.mll"
         
# 360 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )

# 1428 "lib/read.ml"

  
# 1429 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_stringlit_rec v lexbuf __ocaml_lex_state

and finish_variant v lexbuf =
   __ocaml_lex_finish_variant_rec v lexbuf 102
and __ocaml_lex_finish_variant_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 363 "lib/read.mll"
         
# 363 "lib/read.mll"
         ( let x = read_json v lexbuf in
           read_space v lexbuf;
           read_gt v lexbuf;
           Some x )

# 1443 "lib/read.ml"

  
# 1444 "lib/read.ml"
  | 1 ->

# 367 "lib/read.mll"
         
# 367 "lib/read.mll"
         ( None )

# 1448 "lib/read.ml"

  
# 1449 "lib/read.ml"
  | 2 ->

# 368 "lib/read.mll"
         
# 368 "lib/read.mll"
         ( long_error "Expected ':' or '>' but found" v lexbuf )

# 1453 "lib/read.ml"

  
# 1454 "lib/read.ml"
  | 3 ->

# 369 "lib/read.mll"
         
# 369 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )

# 1458 "lib/read.ml"

  
# 1459 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_variant_rec v lexbuf __ocaml_lex_state

and read_lt v lexbuf =
   __ocaml_lex_read_lt_rec v lexbuf 107
and __ocaml_lex_read_lt_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 372 "lib/read.mll"
             
# 372 "lib/read.mll"
             ( () )

# 1470 "lib/read.ml"

  
# 1471 "lib/read.ml"
  | 1 ->

# 373 "lib/read.mll"
             
# 373 "lib/read.mll"
             ( long_error "Expected '<' but found" v lexbuf )

# 1475 "lib/read.ml"

  
# 1476 "lib/read.ml"
  | 2 ->

# 374 "lib/read.mll"
             
# 374 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 1480 "lib/read.ml"

  
# 1481 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_lt_rec v lexbuf __ocaml_lex_state

and read_gt v lexbuf =
   __ocaml_lex_read_gt_rec v lexbuf 111
and __ocaml_lex_read_gt_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 377 "lib/read.mll"
         
# 377 "lib/read.mll"
         ( () )

# 1492 "lib/read.ml"

  
# 1493 "lib/read.ml"
  | 1 ->

# 378 "lib/read.mll"
         
# 378 "lib/read.mll"
         ( long_error "Expected '>' but found" v lexbuf )

# 1497 "lib/read.ml"

  
# 1498 "lib/read.ml"
  | 2 ->

# 379 "lib/read.mll"
         
# 379 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )

# 1502 "lib/read.ml"

  
# 1503 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_gt_rec v lexbuf __ocaml_lex_state

and read_comma v lexbuf =
   __ocaml_lex_read_comma_rec v lexbuf 115
and __ocaml_lex_read_comma_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 382 "lib/read.mll"
         
# 382 "lib/read.mll"
         ( () )

# 1514 "lib/read.ml"

  
# 1515 "lib/read.ml"
  | 1 ->

# 383 "lib/read.mll"
         
# 383 "lib/read.mll"
         ( long_error "Expected ',' but found" v lexbuf )

# 1519 "lib/read.ml"

  
# 1520 "lib/read.ml"
  | 2 ->

# 384 "lib/read.mll"
         
# 384 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )

# 1524 "lib/read.ml"

  
# 1525 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_comma_rec v lexbuf __ocaml_lex_state

and start_any_variant v lexbuf =
   __ocaml_lex_start_any_variant_rec v lexbuf 119
and __ocaml_lex_start_any_variant_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 387 "lib/read.mll"
             
# 387 "lib/read.mll"
             ( `Edgy_bracket )

# 1536 "lib/read.ml"

  
# 1537 "lib/read.ml"
  | 1 ->

# 388 "lib/read.mll"
             
# 388 "lib/read.mll"
             ( Buffer.clear v.buf;
               `Double_quote )

# 1542 "lib/read.ml"

  
# 1543 "lib/read.ml"
  | 2 ->

# 390 "lib/read.mll"
             
# 390 "lib/read.mll"
             ( `Square_bracket )

# 1547 "lib/read.ml"

  
# 1548 "lib/read.ml"
  | 3 ->

# 391 "lib/read.mll"
             
# 391 "lib/read.mll"
             ( long_error "Expected '<', '\"' or '[' but found" v lexbuf )

# 1552 "lib/read.ml"

  
# 1553 "lib/read.ml"
  | 4 ->

# 392 "lib/read.mll"
             
# 392 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 1557 "lib/read.ml"

  
# 1558 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_start_any_variant_rec v lexbuf __ocaml_lex_state

and finish_comment v lexbuf =
   __ocaml_lex_finish_comment_rec v lexbuf 125
and __ocaml_lex_finish_comment_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 395 "lib/read.mll"
         
# 395 "lib/read.mll"
         ( () )

# 1569 "lib/read.ml"

  
# 1570 "lib/read.ml"
  | 1 ->

# 396 "lib/read.mll"
         
# 396 "lib/read.mll"
         ( long_error "Unterminated comment" v lexbuf )

# 1574 "lib/read.ml"

  
# 1575 "lib/read.ml"
  | 2 ->

# 397 "lib/read.mll"
         
# 397 "lib/read.mll"
         ( newline v lexbuf; finish_comment v lexbuf )

# 1579 "lib/read.ml"

  
# 1580 "lib/read.ml"
  | 3 ->

# 398 "lib/read.mll"
         
# 398 "lib/read.mll"
         ( finish_comment v lexbuf )

# 1584 "lib/read.ml"

  
# 1585 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_comment_rec v lexbuf __ocaml_lex_state

and read_eof lexbuf =
   __ocaml_lex_read_eof_rec lexbuf 131
and __ocaml_lex_read_eof_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 406 "lib/read.mll"
              
# 406 "lib/read.mll"
              ( true )

# 1596 "lib/read.ml"

  
# 1597 "lib/read.ml"
  | 1 ->

# 407 "lib/read.mll"
              
# 407 "lib/read.mll"
              ( false )

# 1601 "lib/read.ml"

  
# 1602 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_eof_rec lexbuf __ocaml_lex_state

and read_space v lexbuf =
   __ocaml_lex_read_space_rec v lexbuf 133
and __ocaml_lex_read_space_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 410 "lib/read.mll"
                             
# 410 "lib/read.mll"
                             ( newline v lexbuf; read_space v lexbuf )

# 1613 "lib/read.ml"

  
# 1614 "lib/read.ml"
  | 1 ->

# 411 "lib/read.mll"
                             
# 411 "lib/read.mll"
                             ( finish_comment v lexbuf; read_space v lexbuf )

# 1618 "lib/read.ml"

  
# 1619 "lib/read.ml"
  | 2 ->

# 412 "lib/read.mll"
                             
# 412 "lib/read.mll"
                             ( newline v lexbuf; read_space v lexbuf )

# 1623 "lib/read.ml"

  
# 1624 "lib/read.ml"
  | 3 ->

# 413 "lib/read.mll"
                             
# 413 "lib/read.mll"
                             ( read_space v lexbuf )

# 1628 "lib/read.ml"

  
# 1629 "lib/read.ml"
  | 4 ->

# 414 "lib/read.mll"
                             
# 414 "lib/read.mll"
                             ( () )

# 1633 "lib/read.ml"

  
# 1634 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_space_rec v lexbuf __ocaml_lex_state

and read_null v lexbuf =
   __ocaml_lex_read_null_rec v lexbuf 140
and __ocaml_lex_read_null_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 417 "lib/read.mll"
              
# 417 "lib/read.mll"
              ( () )

# 1645 "lib/read.ml"

  
# 1646 "lib/read.ml"
  | 1 ->

# 418 "lib/read.mll"
              
# 418 "lib/read.mll"
              ( long_error "Expected 'null' but found" v lexbuf )

# 1650 "lib/read.ml"

  
# 1651 "lib/read.ml"
  | 2 ->

# 419 "lib/read.mll"
              
# 419 "lib/read.mll"
              ( custom_error "Unexpected end of input" v lexbuf )

# 1655 "lib/read.ml"

  
# 1656 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_null_rec v lexbuf __ocaml_lex_state

and read_null_if_possible v lexbuf =
   __ocaml_lex_read_null_if_possible_rec v lexbuf 147
and __ocaml_lex_read_null_if_possible_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 422 "lib/read.mll"
              
# 422 "lib/read.mll"
              ( true )

# 1667 "lib/read.ml"

  
# 1668 "lib/read.ml"
  | 1 ->

# 423 "lib/read.mll"
              
# 423 "lib/read.mll"
              ( false )

# 1672 "lib/read.ml"

  
# 1673 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_null_if_possible_rec v lexbuf __ocaml_lex_state

and read_bool v lexbuf =
   __ocaml_lex_read_bool_rec v lexbuf 152
and __ocaml_lex_read_bool_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 426 "lib/read.mll"
                
# 426 "lib/read.mll"
                ( true )

# 1684 "lib/read.ml"

  
# 1685 "lib/read.ml"
  | 1 ->

# 427 "lib/read.mll"
                
# 427 "lib/read.mll"
                ( false )

# 1689 "lib/read.ml"

  
# 1690 "lib/read.ml"
  | 2 ->

# 430 "lib/read.mll"
                
# 430 "lib/read.mll"
                ( true )

# 1694 "lib/read.ml"

  
# 1695 "lib/read.ml"
  | 3 ->

# 431 "lib/read.mll"
                
# 431 "lib/read.mll"
                ( false )

# 1699 "lib/read.ml"

  
# 1700 "lib/read.ml"
  | 4 ->

# 433 "lib/read.mll"
                
# 433 "lib/read.mll"
                ( long_error "Expected 'true' or 'false' but found" v lexbuf )

# 1704 "lib/read.ml"

  
# 1705 "lib/read.ml"
  | 5 ->

# 434 "lib/read.mll"
                
# 434 "lib/read.mll"
                ( custom_error "Unexpected end of input" v lexbuf )

# 1709 "lib/read.ml"

  
# 1710 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_bool_rec v lexbuf __ocaml_lex_state

and read_int v lexbuf =
   __ocaml_lex_read_int_rec v lexbuf 176
and __ocaml_lex_read_int_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 437 "lib/read.mll"
                         
# 437 "lib/read.mll"
                         ( try extract_positive_int lexbuf
                           with Int_overflow ->
                             lexer_error "Int overflow" v lexbuf )

# 1723 "lib/read.ml"

  
# 1724 "lib/read.ml"
  | 1 ->

# 440 "lib/read.mll"
                         
# 440 "lib/read.mll"
                         ( try extract_negative_int lexbuf
                           with Int_overflow ->
                             lexer_error "Int overflow" v lexbuf )

# 1730 "lib/read.ml"

  
# 1731 "lib/read.ml"
  | 2 ->

# 443 "lib/read.mll"
                         
# 443 "lib/read.mll"
                         ( (* Support for double-quoted "ints" *)
                           Buffer.clear v.buf;
                           let s = finish_string v lexbuf in
                           try
                             (* Any OCaml-compliant int will pass,
                                including hexadecimal and octal notations,
                                and embedded underscores *)
                             int_of_string s
                           with _ ->
                             custom_error
                               "Expected an integer but found a string that \
                                doesn't even represent an integer"
                               v lexbuf
                         )

# 1748 "lib/read.ml"

  
# 1749 "lib/read.ml"
  | 3 ->

# 457 "lib/read.mll"
                         
# 457 "lib/read.mll"
                         ( long_error "Expected integer but found" v lexbuf )

# 1753 "lib/read.ml"

  
# 1754 "lib/read.ml"
  | 4 ->

# 458 "lib/read.mll"
                         
# 458 "lib/read.mll"
                         ( custom_error "Unexpected end of input" v lexbuf )

# 1758 "lib/read.ml"

  
# 1759 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_int_rec v lexbuf __ocaml_lex_state

and read_int32 v lexbuf =
   __ocaml_lex_read_int32_rec v lexbuf 185
and __ocaml_lex_read_int32_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 461 "lib/read.mll"
                         
# 461 "lib/read.mll"
                         ( try Int32.of_string (Lexing.lexeme lexbuf)
                           with _ ->
                             lexer_error "Int32 overflow" v lexbuf )

# 1772 "lib/read.ml"

  
# 1773 "lib/read.ml"
  | 1 ->

# 464 "lib/read.mll"
                         
# 464 "lib/read.mll"
                         ( (* Support for double-quoted "ints" *)
                           Buffer.clear v.buf;
                           let s = finish_string v lexbuf in
                           try
                             (* Any OCaml-compliant int will pass,
                                including hexadecimal and octal notations,
                                and embedded underscores *)
                             Int32.of_string s
                           with _ ->
                             custom_error
                               "Expected an int32 but found a string that \
                                doesn't even represent an integer"
                               v lexbuf
                         )

# 1790 "lib/read.ml"

  
# 1791 "lib/read.ml"
  | 2 ->

# 478 "lib/read.mll"
                         
# 478 "lib/read.mll"
                         ( long_error "Expected int32 but found" v lexbuf )

# 1795 "lib/read.ml"

  
# 1796 "lib/read.ml"
  | 3 ->

# 479 "lib/read.mll"
                         
# 479 "lib/read.mll"
                         ( custom_error "Unexpected end of input" v lexbuf )

# 1800 "lib/read.ml"

  
# 1801 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_int32_rec v lexbuf __ocaml_lex_state

and read_int64 v lexbuf =
   __ocaml_lex_read_int64_rec v lexbuf 192
and __ocaml_lex_read_int64_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 482 "lib/read.mll"
                         
# 482 "lib/read.mll"
                         ( try Int64.of_string (Lexing.lexeme lexbuf)
                           with _ ->
                             lexer_error "Int32 overflow" v lexbuf )

# 1814 "lib/read.ml"

  
# 1815 "lib/read.ml"
  | 1 ->

# 485 "lib/read.mll"
                         
# 485 "lib/read.mll"
                         ( (* Support for double-quoted "ints" *)
                           Buffer.clear v.buf;
                           let s = finish_string v lexbuf in
                           try
                             (* Any OCaml-compliant int will pass,
                                including hexadecimal and octal notations,
                                and embedded underscores *)
                             Int64.of_string s
                           with _ ->
                             custom_error
                               "Expected an int64 but found a string that \
                                doesn't even represent an integer"
                               v lexbuf
                         )

# 1832 "lib/read.ml"

  
# 1833 "lib/read.ml"
  | 2 ->

# 499 "lib/read.mll"
                         
# 499 "lib/read.mll"
                         ( long_error "Expected int64 but found" v lexbuf )

# 1837 "lib/read.ml"

  
# 1838 "lib/read.ml"
  | 3 ->

# 500 "lib/read.mll"
                         
# 500 "lib/read.mll"
                         ( custom_error "Unexpected end of input" v lexbuf )

# 1842 "lib/read.ml"

  
# 1843 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_int64_rec v lexbuf __ocaml_lex_state

and read_number v lexbuf =
   __ocaml_lex_read_number_rec v lexbuf 199
and __ocaml_lex_read_number_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 503 "lib/read.mll"
                
# 503 "lib/read.mll"
                ( nan )

# 1854 "lib/read.ml"

  
# 1855 "lib/read.ml"
  | 1 ->

# 504 "lib/read.mll"
                
# 504 "lib/read.mll"
                ( infinity )

# 1859 "lib/read.ml"

  
# 1860 "lib/read.ml"
  | 2 ->

# 505 "lib/read.mll"
                
# 505 "lib/read.mll"
                ( neg_infinity )

# 1864 "lib/read.ml"

  
# 1865 "lib/read.ml"
  | 3 ->

# 506 "lib/read.mll"
                
# 506 "lib/read.mll"
                ( float_of_string (lexeme lexbuf) )

# 1869 "lib/read.ml"

  
# 1870 "lib/read.ml"
  | 4 ->

# 507 "lib/read.mll"
                
# 507 "lib/read.mll"
                ( Buffer.clear v.buf;
                  let s = finish_string v lexbuf in
                  try
                    (* Any OCaml-compliant float will pass,
                       including hexadecimal and octal notations,
                       and embedded underscores. *)
                    float_of_string s
                  with _ ->
                    match s with
                        "NaN" -> nan
                      | "Infinity" -> infinity
                      | "-Infinity" -> neg_infinity
                      | _ ->
                          custom_error
                            "Expected a number but found a string that \
                             doesn't even represent a number"
                            v lexbuf
                )

# 1891 "lib/read.ml"

  
# 1892 "lib/read.ml"
  | 5 ->

# 525 "lib/read.mll"
                
# 525 "lib/read.mll"
                ( long_error "Expected number but found" v lexbuf )

# 1896 "lib/read.ml"

  
# 1897 "lib/read.ml"
  | 6 ->

# 526 "lib/read.mll"
                
# 526 "lib/read.mll"
                ( custom_error "Unexpected end of input" v lexbuf )

# 1901 "lib/read.ml"

  
# 1902 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_number_rec v lexbuf __ocaml_lex_state

and read_string v lexbuf =
   __ocaml_lex_read_string_rec v lexbuf 233
and __ocaml_lex_read_string_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 529 "lib/read.mll"
             
# 529 "lib/read.mll"
             ( Buffer.clear v.buf;
               finish_string v lexbuf )

# 1914 "lib/read.ml"

  
# 1915 "lib/read.ml"
  | 1 ->

# 531 "lib/read.mll"
             
# 531 "lib/read.mll"
             ( long_error "Expected '\"' but found" v lexbuf )

# 1919 "lib/read.ml"

  
# 1920 "lib/read.ml"
  | 2 ->

# 532 "lib/read.mll"
             
# 532 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 1924 "lib/read.ml"

  
# 1925 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_string_rec v lexbuf __ocaml_lex_state

and read_ident v lexbuf =
   __ocaml_lex_read_ident_rec v lexbuf 237
and __ocaml_lex_read_ident_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 535 "lib/read.mll"
             
# 535 "lib/read.mll"
             ( Buffer.clear v.buf;
               finish_string v lexbuf )

# 1937 "lib/read.ml"

  
# 1938 "lib/read.ml"
  | 1 ->
let

# 537 "lib/read.mll"
             
# 537 "lib/read.mll"
             s

# 1943 "lib/read.ml"
# 1943 "lib/read.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in

# 538 "lib/read.mll"
             
# 538 "lib/read.mll"
             ( s )

# 1947 "lib/read.ml"

  
# 1948 "lib/read.ml"
  | 2 ->

# 539 "lib/read.mll"
             
# 539 "lib/read.mll"
             ( long_error "Expected string or identifier but found" v lexbuf )

# 1952 "lib/read.ml"

  
# 1953 "lib/read.ml"
  | 3 ->

# 540 "lib/read.mll"
             
# 540 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 1957 "lib/read.ml"

  
# 1958 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_ident_rec v lexbuf __ocaml_lex_state

and map_ident v f lexbuf =
   __ocaml_lex_map_ident_rec v f lexbuf 242
and __ocaml_lex_map_ident_rec v f lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 543 "lib/read.mll"
             
# 543 "lib/read.mll"
             ( Buffer.clear v.buf;
               map_string v f lexbuf )

# 1970 "lib/read.ml"

  
# 1971 "lib/read.ml"
  | 1 ->

# 546 "lib/read.mll"
             
# 546 "lib/read.mll"
             ( map_lexeme f lexbuf )

# 1975 "lib/read.ml"

  
# 1976 "lib/read.ml"
  | 2 ->

# 547 "lib/read.mll"
             
# 547 "lib/read.mll"
             ( long_error "Expected string or identifier but found" v lexbuf )

# 1980 "lib/read.ml"

  
# 1981 "lib/read.ml"
  | 3 ->

# 548 "lib/read.mll"
             
# 548 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 1985 "lib/read.ml"

  
# 1986 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_map_ident_rec v f lexbuf __ocaml_lex_state

and read_sequence read_cell init_acc v lexbuf =
   __ocaml_lex_read_sequence_rec read_cell init_acc v lexbuf 247
and __ocaml_lex_read_sequence_rec read_cell init_acc v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 551 "lib/read.mll"
             
# 551 "lib/read.mll"
             ( let acc = ref init_acc in
               try
                 read_space v lexbuf;
                 read_array_end lexbuf;
                 acc := read_cell !acc v lexbuf;
                 while true do
                   read_space v lexbuf;
                   read_array_sep v lexbuf;
                   read_space v lexbuf;
                   acc := read_cell !acc v lexbuf;
                 done;
                 assert false
               with Common.End_of_array ->
                 !acc
             )

# 2011 "lib/read.ml"

  
# 2012 "lib/read.ml"
  | 1 ->

# 566 "lib/read.mll"
             
# 566 "lib/read.mll"
             ( long_error "Expected '[' but found" v lexbuf )

# 2016 "lib/read.ml"

  
# 2017 "lib/read.ml"
  | 2 ->

# 567 "lib/read.mll"
             
# 567 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2021 "lib/read.ml"

  
# 2022 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_sequence_rec read_cell init_acc v lexbuf __ocaml_lex_state

and read_list_rev read_cell v lexbuf =
   __ocaml_lex_read_list_rev_rec read_cell v lexbuf 251
and __ocaml_lex_read_list_rev_rec read_cell v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 570 "lib/read.mll"
             
# 570 "lib/read.mll"
             ( let acc = ref [] in
               try
                 read_space v lexbuf;
                 read_array_end lexbuf;
                 acc := read_cell v lexbuf :: !acc;
                 while true do
                   read_space v lexbuf;
                   read_array_sep v lexbuf;
                   read_space v lexbuf;
                   acc := read_cell v lexbuf :: !acc;
                 done;
                 assert false
               with Common.End_of_array ->
                 !acc
             )

# 2047 "lib/read.ml"

  
# 2048 "lib/read.ml"
  | 1 ->

# 585 "lib/read.mll"
             
# 585 "lib/read.mll"
             ( long_error "Expected '[' but found" v lexbuf )

# 2052 "lib/read.ml"

  
# 2053 "lib/read.ml"
  | 2 ->

# 586 "lib/read.mll"
             
# 586 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2057 "lib/read.ml"

  
# 2058 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_list_rev_rec read_cell v lexbuf __ocaml_lex_state

and read_array_end lexbuf =
   __ocaml_lex_read_array_end_rec lexbuf 255
and __ocaml_lex_read_array_end_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 589 "lib/read.mll"
             
# 589 "lib/read.mll"
             ( raise Common.End_of_array )

# 2069 "lib/read.ml"

  
# 2070 "lib/read.ml"
  | 1 ->

# 590 "lib/read.mll"
             
# 590 "lib/read.mll"
             ( () )

# 2074 "lib/read.ml"

  
# 2075 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_array_end_rec lexbuf __ocaml_lex_state

and read_array_sep v lexbuf =
   __ocaml_lex_read_array_sep_rec v lexbuf 257
and __ocaml_lex_read_array_sep_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 593 "lib/read.mll"
             
# 593 "lib/read.mll"
             ( () )

# 2086 "lib/read.ml"

  
# 2087 "lib/read.ml"
  | 1 ->

# 594 "lib/read.mll"
             
# 594 "lib/read.mll"
             ( raise Common.End_of_array )

# 2091 "lib/read.ml"

  
# 2092 "lib/read.ml"
  | 2 ->

# 595 "lib/read.mll"
             
# 595 "lib/read.mll"
             ( long_error "Expected ',' or ']' but found" v lexbuf )

# 2096 "lib/read.ml"

  
# 2097 "lib/read.ml"
  | 3 ->

# 596 "lib/read.mll"
             
# 596 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2101 "lib/read.ml"

  
# 2102 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_array_sep_rec v lexbuf __ocaml_lex_state

and read_tuple read_cell init_acc v lexbuf =
   __ocaml_lex_read_tuple_rec read_cell init_acc v lexbuf 262
and __ocaml_lex_read_tuple_rec read_cell init_acc v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 600 "lib/read.mll"
                 
# 600 "lib/read.mll"
                 (
                     
# 620 "lib/read.mll"
                     long_error "Invalid token" v lexbuf
                 
# 622 "lib/read.mll"
                 )

# 2135 "lib/read.ml"

  
# 2136 "lib/read.ml"
  | 1 ->

# 623 "lib/read.mll"
             
# 623 "lib/read.mll"
             ( long_error "Expected ')' but found" v lexbuf )

# 2140 "lib/read.ml"

  
# 2141 "lib/read.ml"
  | 2 ->

# 624 "lib/read.mll"
             
# 624 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2145 "lib/read.ml"

  
# 2146 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_tuple_rec read_cell init_acc v lexbuf __ocaml_lex_state

and read_tuple_end lexbuf =
   __ocaml_lex_read_tuple_end_rec lexbuf 266
and __ocaml_lex_read_tuple_end_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 627 "lib/read.mll"
             
# 627 "lib/read.mll"
             ( raise Common.End_of_tuple )

# 2157 "lib/read.ml"

  
# 2158 "lib/read.ml"
  | 1 ->

# 628 "lib/read.mll"
             
# 628 "lib/read.mll"
             ( () )

# 2162 "lib/read.ml"

  
# 2163 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_tuple_end_rec lexbuf __ocaml_lex_state

and read_tuple_end2 v std lexbuf =
   __ocaml_lex_read_tuple_end2_rec v std lexbuf 268
and __ocaml_lex_read_tuple_end2_rec v std lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 631 "lib/read.mll"
             
# 631 "lib/read.mll"
             ( if std then
                 long_error "Expected ')' or '' but found" v lexbuf
               else
                 raise Common.End_of_tuple )

# 2177 "lib/read.ml"

  
# 2178 "lib/read.ml"
  | 1 ->

# 635 "lib/read.mll"
             
# 635 "lib/read.mll"
             ( if std then
                 raise Common.End_of_tuple
               else
                 long_error "Expected ']' or '' but found" v lexbuf )

# 2185 "lib/read.ml"

  
# 2186 "lib/read.ml"
  | 2 ->

# 639 "lib/read.mll"
             
# 639 "lib/read.mll"
             ( () )

# 2190 "lib/read.ml"

  
# 2191 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_tuple_end2_rec v std lexbuf __ocaml_lex_state

and read_tuple_sep v lexbuf =
   __ocaml_lex_read_tuple_sep_rec v lexbuf 271
and __ocaml_lex_read_tuple_sep_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 642 "lib/read.mll"
             
# 642 "lib/read.mll"
             ( () )

# 2202 "lib/read.ml"

  
# 2203 "lib/read.ml"
  | 1 ->

# 643 "lib/read.mll"
             
# 643 "lib/read.mll"
             ( raise Common.End_of_tuple )

# 2207 "lib/read.ml"

  
# 2208 "lib/read.ml"
  | 2 ->

# 644 "lib/read.mll"
             
# 644 "lib/read.mll"
             ( long_error "Expected ',' or ')' but found" v lexbuf )

# 2212 "lib/read.ml"

  
# 2213 "lib/read.ml"
  | 3 ->

# 645 "lib/read.mll"
             
# 645 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2217 "lib/read.ml"

  
# 2218 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_tuple_sep_rec v lexbuf __ocaml_lex_state

and read_tuple_sep2 v std lexbuf =
   __ocaml_lex_read_tuple_sep2_rec v std lexbuf 276
and __ocaml_lex_read_tuple_sep2_rec v std lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 648 "lib/read.mll"
             
# 648 "lib/read.mll"
             ( () )

# 2229 "lib/read.ml"

  
# 2230 "lib/read.ml"
  | 1 ->

# 649 "lib/read.mll"
             
# 649 "lib/read.mll"
             ( if std then
                 long_error "Expected ',' or ']' but found" v lexbuf
               else
                 raise Common.End_of_tuple )

# 2237 "lib/read.ml"

  
# 2238 "lib/read.ml"
  | 2 ->

# 653 "lib/read.mll"
             
# 653 "lib/read.mll"
             ( if std then
                 raise Common.End_of_tuple
               else
                 long_error "Expected ',' or ')' but found" v lexbuf )

# 2245 "lib/read.ml"

  
# 2246 "lib/read.ml"
  | 3 ->

# 657 "lib/read.mll"
             
# 657 "lib/read.mll"
             ( long_error "Expected ',' or ')' but found" v lexbuf )

# 2250 "lib/read.ml"

  
# 2251 "lib/read.ml"
  | 4 ->

# 658 "lib/read.mll"
             
# 658 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2255 "lib/read.ml"

  
# 2256 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_tuple_sep2_rec v std lexbuf __ocaml_lex_state

and read_abstract_fields read_key read_field init_acc v lexbuf =
   __ocaml_lex_read_abstract_fields_rec read_key read_field init_acc v lexbuf 282
and __ocaml_lex_read_abstract_fields_rec read_key read_field init_acc v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 662 "lib/read.mll"
             
# 662 "lib/read.mll"
             ( let acc = ref init_acc in
               try
                 read_space v lexbuf;
                 read_object_end lexbuf;
                 let field_name = read_key v lexbuf in
                 read_space v lexbuf;
                 read_colon v lexbuf;
                 read_space v lexbuf;
                 acc := read_field !acc field_name v lexbuf;
                 while true do
                   read_space v lexbuf;
                   read_object_sep v lexbuf;
                   read_space v lexbuf;
                   let field_name = read_key v lexbuf in
                   read_space v lexbuf;
                   read_colon v lexbuf;
                   read_space v lexbuf;
                   acc := read_field !acc field_name v lexbuf;
                 done;
                 assert false
               with Common.End_of_object ->
                 !acc
             )

# 2289 "lib/read.ml"

  
# 2290 "lib/read.ml"
  | 1 ->

# 685 "lib/read.mll"
             
# 685 "lib/read.mll"
             ( long_error "Expected '{' but found" v lexbuf )

# 2294 "lib/read.ml"

  
# 2295 "lib/read.ml"
  | 2 ->

# 686 "lib/read.mll"
             
# 686 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2299 "lib/read.ml"

  
# 2300 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_abstract_fields_rec read_key read_field init_acc v lexbuf __ocaml_lex_state

and read_lcurl v lexbuf =
   __ocaml_lex_read_lcurl_rec v lexbuf 286
and __ocaml_lex_read_lcurl_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 689 "lib/read.mll"
             
# 689 "lib/read.mll"
             ( () )

# 2311 "lib/read.ml"

  
# 2312 "lib/read.ml"
  | 1 ->

# 690 "lib/read.mll"
             
# 690 "lib/read.mll"
             ( long_error "Expected '{' but found" v lexbuf )

# 2316 "lib/read.ml"

  
# 2317 "lib/read.ml"
  | 2 ->

# 691 "lib/read.mll"
             
# 691 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2321 "lib/read.ml"

  
# 2322 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_lcurl_rec v lexbuf __ocaml_lex_state

and read_object_end lexbuf =
   __ocaml_lex_read_object_end_rec lexbuf 290
and __ocaml_lex_read_object_end_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 694 "lib/read.mll"
             
# 694 "lib/read.mll"
             ( raise Common.End_of_object )

# 2333 "lib/read.ml"

  
# 2334 "lib/read.ml"
  | 1 ->

# 695 "lib/read.mll"
             
# 695 "lib/read.mll"
             ( () )

# 2338 "lib/read.ml"

  
# 2339 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_object_end_rec lexbuf __ocaml_lex_state

and read_object_sep v lexbuf =
   __ocaml_lex_read_object_sep_rec v lexbuf 292
and __ocaml_lex_read_object_sep_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 698 "lib/read.mll"
             
# 698 "lib/read.mll"
             ( () )

# 2350 "lib/read.ml"

  
# 2351 "lib/read.ml"
  | 1 ->

# 699 "lib/read.mll"
             
# 699 "lib/read.mll"
             ( raise Common.End_of_object )

# 2355 "lib/read.ml"

  
# 2356 "lib/read.ml"
  | 2 ->

# 700 "lib/read.mll"
             
# 700 "lib/read.mll"
             ( long_error "Expected ',' or '}' but found" v lexbuf )

# 2360 "lib/read.ml"

  
# 2361 "lib/read.ml"
  | 3 ->

# 701 "lib/read.mll"
             
# 701 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2365 "lib/read.ml"

  
# 2366 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_object_sep_rec v lexbuf __ocaml_lex_state

and read_colon v lexbuf =
   __ocaml_lex_read_colon_rec v lexbuf 297
and __ocaml_lex_read_colon_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 704 "lib/read.mll"
             
# 704 "lib/read.mll"
             ( () )

# 2377 "lib/read.ml"

  
# 2378 "lib/read.ml"
  | 1 ->

# 705 "lib/read.mll"
             
# 705 "lib/read.mll"
             ( long_error "Expected ':' but found" v lexbuf )

# 2382 "lib/read.ml"

  
# 2383 "lib/read.ml"
  | 2 ->

# 706 "lib/read.mll"
             
# 706 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2387 "lib/read.ml"

  
# 2388 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_colon_rec v lexbuf __ocaml_lex_state

and start_any_tuple v lexbuf =
   __ocaml_lex_start_any_tuple_rec v lexbuf 301
and __ocaml_lex_start_any_tuple_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 709 "lib/read.mll"
             
# 709 "lib/read.mll"
             ( false )

# 2399 "lib/read.ml"

  
# 2400 "lib/read.ml"
  | 1 ->

# 710 "lib/read.mll"
             
# 710 "lib/read.mll"
             ( true )

# 2404 "lib/read.ml"

  
# 2405 "lib/read.ml"
  | 2 ->

# 711 "lib/read.mll"
             
# 711 "lib/read.mll"
             ( long_error "Expected '(' or '[' but found" v lexbuf )

# 2409 "lib/read.ml"

  
# 2410 "lib/read.ml"
  | 3 ->

# 712 "lib/read.mll"
             
# 712 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2414 "lib/read.ml"

  
# 2415 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_start_any_tuple_rec v lexbuf __ocaml_lex_state

and read_lpar v lexbuf =
   __ocaml_lex_read_lpar_rec v lexbuf 306
and __ocaml_lex_read_lpar_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 715 "lib/read.mll"
             
# 715 "lib/read.mll"
             ( () )

# 2426 "lib/read.ml"

  
# 2427 "lib/read.ml"
  | 1 ->

# 716 "lib/read.mll"
             
# 716 "lib/read.mll"
             ( long_error "Expected '(' but found" v lexbuf )

# 2431 "lib/read.ml"

  
# 2432 "lib/read.ml"
  | 2 ->

# 717 "lib/read.mll"
             
# 717 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2436 "lib/read.ml"

  
# 2437 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_lpar_rec v lexbuf __ocaml_lex_state

and read_rpar v lexbuf =
   __ocaml_lex_read_rpar_rec v lexbuf 310
and __ocaml_lex_read_rpar_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 720 "lib/read.mll"
             
# 720 "lib/read.mll"
             ( () )

# 2448 "lib/read.ml"

  
# 2449 "lib/read.ml"
  | 1 ->

# 721 "lib/read.mll"
             
# 721 "lib/read.mll"
             ( long_error "Expected ')' but found" v lexbuf )

# 2453 "lib/read.ml"

  
# 2454 "lib/read.ml"
  | 2 ->

# 722 "lib/read.mll"
             
# 722 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2458 "lib/read.ml"

  
# 2459 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_rpar_rec v lexbuf __ocaml_lex_state

and read_lbr v lexbuf =
   __ocaml_lex_read_lbr_rec v lexbuf 314
and __ocaml_lex_read_lbr_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 725 "lib/read.mll"
             
# 725 "lib/read.mll"
             ( () )

# 2470 "lib/read.ml"

  
# 2471 "lib/read.ml"
  | 1 ->

# 726 "lib/read.mll"
             
# 726 "lib/read.mll"
             ( long_error "Expected '[' but found" v lexbuf )

# 2475 "lib/read.ml"

  
# 2476 "lib/read.ml"
  | 2 ->

# 727 "lib/read.mll"
             
# 727 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2480 "lib/read.ml"

  
# 2481 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_lbr_rec v lexbuf __ocaml_lex_state

and read_rbr v lexbuf =
   __ocaml_lex_read_rbr_rec v lexbuf 318
and __ocaml_lex_read_rbr_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 730 "lib/read.mll"
             
# 730 "lib/read.mll"
             ( () )

# 2492 "lib/read.ml"

  
# 2493 "lib/read.ml"
  | 1 ->

# 731 "lib/read.mll"
             
# 731 "lib/read.mll"
             ( long_error "Expected ']' but found" v lexbuf )

# 2497 "lib/read.ml"

  
# 2498 "lib/read.ml"
  | 2 ->

# 732 "lib/read.mll"
             
# 732 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2502 "lib/read.ml"

  
# 2503 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_rbr_rec v lexbuf __ocaml_lex_state

and skip_json v lexbuf =
   __ocaml_lex_skip_json_rec v lexbuf 322
and __ocaml_lex_skip_json_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 738 "lib/read.mll"
                
# 738 "lib/read.mll"
                ( () )

# 2514 "lib/read.ml"

  
# 2515 "lib/read.ml"
  | 1 ->

# 739 "lib/read.mll"
                
# 739 "lib/read.mll"
                ( () )

# 2519 "lib/read.ml"

  
# 2520 "lib/read.ml"
  | 2 ->

# 740 "lib/read.mll"
                
# 740 "lib/read.mll"
                ( () )

# 2524 "lib/read.ml"

  
# 2525 "lib/read.ml"
  | 3 ->

# 741 "lib/read.mll"
                
# 741 "lib/read.mll"
                ( () )

# 2529 "lib/read.ml"

  
# 2530 "lib/read.ml"
  | 4 ->

# 742 "lib/read.mll"
                
# 742 "lib/read.mll"
                ( () )

# 2534 "lib/read.ml"

  
# 2535 "lib/read.ml"
  | 5 ->

# 743 "lib/read.mll"
                
# 743 "lib/read.mll"
                ( () )

# 2539 "lib/read.ml"

  
# 2540 "lib/read.ml"
  | 6 ->

# 744 "lib/read.mll"
                
# 744 "lib/read.mll"
                ( finish_skip_stringlit v lexbuf )

# 2544 "lib/read.ml"

  
# 2545 "lib/read.ml"
  | 7 ->

# 745 "lib/read.mll"
                          
# 745 "lib/read.mll"
                          ( () )

# 2549 "lib/read.ml"

  
# 2550 "lib/read.ml"
  | 8 ->

# 746 "lib/read.mll"
                
# 746 "lib/read.mll"
                ( () )

# 2554 "lib/read.ml"

  
# 2555 "lib/read.ml"
  | 9 ->

# 748 "lib/read.mll"
                 
# 748 "lib/read.mll"
                 ( try
                     read_space v lexbuf;
                     read_object_end lexbuf;
                     skip_ident v lexbuf;
                     read_space v lexbuf;
                     read_colon v lexbuf;
                     read_space v lexbuf;
                     skip_json v lexbuf;
                     while true do
                       read_space v lexbuf;
                       read_object_sep v lexbuf;
                       read_space v lexbuf;
                       skip_ident v lexbuf;
                       read_space v lexbuf;
                       read_colon v lexbuf;
                       read_space v lexbuf;
                       skip_json v lexbuf;
                     done;
                     assert false
                   with Common.End_of_object ->
                     ()
                 )

# 2580 "lib/read.ml"

  
# 2581 "lib/read.ml"
  | 10 ->

# 771 "lib/read.mll"
                 
# 771 "lib/read.mll"
                 ( try
                     read_space v lexbuf;
                     read_array_end lexbuf;
                     skip_json v lexbuf;
                     while true do
                       read_space v lexbuf;
                       read_array_sep v lexbuf;
                       read_space v lexbuf;
                       skip_json v lexbuf;
                     done;
                     assert false
                   with Common.End_of_array ->
                     ()
                 )

# 2598 "lib/read.ml"

  
# 2599 "lib/read.ml"
  | 11 ->

# 786 "lib/read.mll"
                 
# 786 "lib/read.mll"
                 (
                     
# 802 "lib/read.mll"
                     long_error "Invalid token" v lexbuf
                 
# 804 "lib/read.mll"
                 )

# 2621 "lib/read.ml"

  
# 2622 "lib/read.ml"
  | 12 ->

# 806 "lib/read.mll"
                 
# 806 "lib/read.mll"
                 (
                     
# 813 "lib/read.mll"
                     long_error "Invalid token" v lexbuf
                 
# 815 "lib/read.mll"
                 )

# 2635 "lib/read.ml"

  
# 2636 "lib/read.ml"
  | 13 ->

# 817 "lib/read.mll"
                 
# 817 "lib/read.mll"
                 ( skip_json v lexbuf )

# 2640 "lib/read.ml"

  
# 2641 "lib/read.ml"
  | 14 ->

# 818 "lib/read.mll"
                 
# 818 "lib/read.mll"
                 ( finish_comment v lexbuf; skip_json v lexbuf )

# 2645 "lib/read.ml"

  
# 2646 "lib/read.ml"
  | 15 ->

# 819 "lib/read.mll"
                 
# 819 "lib/read.mll"
                 ( newline v lexbuf; skip_json v lexbuf )

# 2650 "lib/read.ml"

  
# 2651 "lib/read.ml"
  | 16 ->

# 820 "lib/read.mll"
                 
# 820 "lib/read.mll"
                 ( skip_json v lexbuf )

# 2655 "lib/read.ml"

  
# 2656 "lib/read.ml"
  | 17 ->

# 821 "lib/read.mll"
                 
# 821 "lib/read.mll"
                 ( custom_error "Unexpected end of input" v lexbuf )

# 2660 "lib/read.ml"

  
# 2661 "lib/read.ml"
  | 18 ->

# 822 "lib/read.mll"
                 
# 822 "lib/read.mll"
                 ( long_error "Invalid token" v lexbuf )

# 2665 "lib/read.ml"

  
# 2666 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_skip_json_rec v lexbuf __ocaml_lex_state

and finish_skip_stringlit v lexbuf =
   __ocaml_lex_finish_skip_stringlit_rec v lexbuf 378
and __ocaml_lex_finish_skip_stringlit_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 828 "lib/read.mll"
         
# 828 "lib/read.mll"
         ( () )

# 2677 "lib/read.ml"

  
# 2678 "lib/read.ml"
  | 1 ->

# 829 "lib/read.mll"
         
# 829 "lib/read.mll"
         ( long_error "Invalid string literal" v lexbuf )

# 2682 "lib/read.ml"

  
# 2683 "lib/read.ml"
  | 2 ->

# 830 "lib/read.mll"
         
# 830 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )

# 2687 "lib/read.ml"

  
# 2688 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_skip_stringlit_rec v lexbuf __ocaml_lex_state

and finish_skip_variant v lexbuf =
   __ocaml_lex_finish_skip_variant_rec v lexbuf 389
and __ocaml_lex_finish_skip_variant_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 833 "lib/read.mll"
         
# 833 "lib/read.mll"
         ( skip_json v lexbuf;
           read_space v lexbuf;
           read_gt v lexbuf )

# 2701 "lib/read.ml"

  
# 2702 "lib/read.ml"
  | 1 ->

# 836 "lib/read.mll"
         
# 836 "lib/read.mll"
         ( () )

# 2706 "lib/read.ml"

  
# 2707 "lib/read.ml"
  | 2 ->

# 837 "lib/read.mll"
         
# 837 "lib/read.mll"
         ( long_error "Expected ':' or '>' but found" v lexbuf )

# 2711 "lib/read.ml"

  
# 2712 "lib/read.ml"
  | 3 ->

# 838 "lib/read.mll"
         
# 838 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )

# 2716 "lib/read.ml"

  
# 2717 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_skip_variant_rec v lexbuf __ocaml_lex_state

and skip_ident v lexbuf =
   __ocaml_lex_skip_ident_rec v lexbuf 394
and __ocaml_lex_skip_ident_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 841 "lib/read.mll"
             
# 841 "lib/read.mll"
             ( finish_skip_stringlit v lexbuf )

# 2728 "lib/read.ml"

  
# 2729 "lib/read.ml"
  | 1 ->

# 842 "lib/read.mll"
             
# 842 "lib/read.mll"
             ( () )

# 2733 "lib/read.ml"

  
# 2734 "lib/read.ml"
  | 2 ->

# 843 "lib/read.mll"
             
# 843 "lib/read.mll"
             ( long_error "Expected string or identifier but found" v lexbuf )

# 2738 "lib/read.ml"

  
# 2739 "lib/read.ml"
  | 3 ->

# 844 "lib/read.mll"
             
# 844 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2743 "lib/read.ml"

  
# 2744 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_skip_ident_rec v lexbuf __ocaml_lex_state

and buffer_json v lexbuf =
   __ocaml_lex_buffer_json_rec v lexbuf 399
and __ocaml_lex_buffer_json_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 857 "lib/read.mll"
                
# 857 "lib/read.mll"
                ( add_lexeme v.buf lexbuf )

# 2755 "lib/read.ml"

  
# 2756 "lib/read.ml"
  | 1 ->

# 859 "lib/read.mll"
                
# 859 "lib/read.mll"
                ( finish_buffer_stringlit v lexbuf )

# 2760 "lib/read.ml"

  
# 2761 "lib/read.ml"
  | 2 ->

# 860 "lib/read.mll"
                 
# 860 "lib/read.mll"
                 ( try
                     Buffer.add_char v.buf '{';
                     buffer_space v lexbuf;
                     buffer_object_end v lexbuf;
                     buffer_ident v lexbuf;
                     buffer_space v lexbuf;
                     buffer_colon v lexbuf;
                     buffer_space v lexbuf;
                     buffer_json v lexbuf;
                     while true do
                       buffer_space v lexbuf;
                       buffer_object_sep v lexbuf;
                       buffer_space v lexbuf;
                       buffer_ident v lexbuf;
                       buffer_space v lexbuf;
                       buffer_colon v lexbuf;
                       buffer_space v lexbuf;
                       buffer_json v lexbuf;
                     done;
                     assert false
                   with Common.End_of_object ->
                     ()
                 )

# 2787 "lib/read.ml"

  
# 2788 "lib/read.ml"
  | 3 ->

# 884 "lib/read.mll"
                 
# 884 "lib/read.mll"
                 ( try
                     Buffer.add_char v.buf '[';
                     buffer_space v lexbuf;
                     buffer_array_end v lexbuf;
                     buffer_json v lexbuf;
                     while true do
                       buffer_space v lexbuf;
                       buffer_array_sep v lexbuf;
                       buffer_space v lexbuf;
                       buffer_json v lexbuf;
                     done;
                     assert false
                   with Common.End_of_array ->
                     ()
                 )

# 2806 "lib/read.ml"

  
# 2807 "lib/read.ml"
  | 4 ->

# 900 "lib/read.mll"
                 
# 900 "lib/read.mll"
                 (
                     
# 917 "lib/read.mll"
                     long_error "Invalid token" v lexbuf
                 
# 919 "lib/read.mll"
                 )

# 2830 "lib/read.ml"

  
# 2831 "lib/read.ml"
  | 5 ->

# 921 "lib/read.mll"
                 
# 921 "lib/read.mll"
                 (
                     
# 929 "lib/read.mll"
                     long_error "Invalid token" v lexbuf
                 
# 931 "lib/read.mll"
                 )

# 2845 "lib/read.ml"

  
# 2846 "lib/read.ml"
  | 6 ->

# 933 "lib/read.mll"
                 
# 933 "lib/read.mll"
                 ( add_lexeme v.buf lexbuf; buffer_json v lexbuf )

# 2850 "lib/read.ml"

  
# 2851 "lib/read.ml"
  | 7 ->

# 934 "lib/read.mll"
                 
# 934 "lib/read.mll"
                 ( Buffer.add_string v.buf "/*";
                   finish_buffer_comment v lexbuf;
                   buffer_json v lexbuf )

# 2857 "lib/read.ml"

  
# 2858 "lib/read.ml"
  | 8 ->

# 937 "lib/read.mll"
                 
# 937 "lib/read.mll"
                 ( Buffer.add_char v.buf '\n';
                   newline v lexbuf;
                   buffer_json v lexbuf )

# 2864 "lib/read.ml"

  
# 2865 "lib/read.ml"
  | 9 ->

# 940 "lib/read.mll"
                 
# 940 "lib/read.mll"
                 ( add_lexeme v.buf lexbuf; buffer_json v lexbuf )

# 2869 "lib/read.ml"

  
# 2870 "lib/read.ml"
  | 10 ->

# 941 "lib/read.mll"
                 
# 941 "lib/read.mll"
                 ( custom_error "Unexpected end of input" v lexbuf )

# 2874 "lib/read.ml"

  
# 2875 "lib/read.ml"
  | 11 ->

# 942 "lib/read.mll"
                 
# 942 "lib/read.mll"
                 ( long_error "Invalid token" v lexbuf )

# 2879 "lib/read.ml"

  
# 2880 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_json_rec v lexbuf __ocaml_lex_state

and finish_buffer_stringlit v lexbuf =
   __ocaml_lex_finish_buffer_stringlit_rec v lexbuf 450
and __ocaml_lex_finish_buffer_stringlit_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 948 "lib/read.mll"
         
# 948 "lib/read.mll"
         ( Buffer.add_char v.buf '"';
           add_lexeme v.buf lexbuf
         )

# 2893 "lib/read.ml"

  
# 2894 "lib/read.ml"
  | 1 ->

# 951 "lib/read.mll"
         
# 951 "lib/read.mll"
         ( long_error "Invalid string literal" v lexbuf )

# 2898 "lib/read.ml"

  
# 2899 "lib/read.ml"
  | 2 ->

# 952 "lib/read.mll"
         
# 952 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )

# 2903 "lib/read.ml"

  
# 2904 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_buffer_stringlit_rec v lexbuf __ocaml_lex_state

and finish_buffer_variant v lexbuf =
   __ocaml_lex_finish_buffer_variant_rec v lexbuf 461
and __ocaml_lex_finish_buffer_variant_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 955 "lib/read.mll"
         
# 955 "lib/read.mll"
         ( Buffer.add_char v.buf ':';
           buffer_json v lexbuf;
           buffer_space v lexbuf;
           buffer_gt v lexbuf )

# 2918 "lib/read.ml"

  
# 2919 "lib/read.ml"
  | 1 ->

# 959 "lib/read.mll"
         
# 959 "lib/read.mll"
         ( Buffer.add_char v.buf '>' )

# 2923 "lib/read.ml"

  
# 2924 "lib/read.ml"
  | 2 ->

# 960 "lib/read.mll"
         
# 960 "lib/read.mll"
         ( long_error "Expected ':' or '>' but found" v lexbuf )

# 2928 "lib/read.ml"

  
# 2929 "lib/read.ml"
  | 3 ->

# 961 "lib/read.mll"
         
# 961 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )

# 2933 "lib/read.ml"

  
# 2934 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_buffer_variant_rec v lexbuf __ocaml_lex_state

and buffer_ident v lexbuf =
   __ocaml_lex_buffer_ident_rec v lexbuf 466
and __ocaml_lex_buffer_ident_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 964 "lib/read.mll"
             
# 964 "lib/read.mll"
             ( finish_buffer_stringlit v lexbuf )

# 2945 "lib/read.ml"

  
# 2946 "lib/read.ml"
  | 1 ->

# 965 "lib/read.mll"
             
# 965 "lib/read.mll"
             ( add_lexeme v.buf lexbuf )

# 2950 "lib/read.ml"

  
# 2951 "lib/read.ml"
  | 2 ->

# 966 "lib/read.mll"
             
# 966 "lib/read.mll"
             ( long_error "Expected string or identifier but found" v lexbuf )

# 2955 "lib/read.ml"

  
# 2956 "lib/read.ml"
  | 3 ->

# 967 "lib/read.mll"
             
# 967 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 2960 "lib/read.ml"

  
# 2961 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_ident_rec v lexbuf __ocaml_lex_state

and buffer_space v lexbuf =
   __ocaml_lex_buffer_space_rec v lexbuf 471
and __ocaml_lex_buffer_space_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 970 "lib/read.mll"
                             
# 970 "lib/read.mll"
                             (
    add_lexeme v.buf lexbuf;
    newline v lexbuf;
    buffer_space v lexbuf )

# 2975 "lib/read.ml"

  
# 2976 "lib/read.ml"
  | 1 ->

# 974 "lib/read.mll"
                             
# 974 "lib/read.mll"
                             (
    Buffer.add_string v.buf "/*";
    finish_buffer_comment v lexbuf;
    buffer_space v lexbuf )

# 2983 "lib/read.ml"

  
# 2984 "lib/read.ml"
  | 2 ->

# 978 "lib/read.mll"
                             
# 978 "lib/read.mll"
                             (
    Buffer.add_char v.buf '\n';
    newline v lexbuf;
    buffer_space v lexbuf )

# 2991 "lib/read.ml"

  
# 2992 "lib/read.ml"
  | 3 ->

# 982 "lib/read.mll"
                             
# 982 "lib/read.mll"
                             (
    add_lexeme v.buf lexbuf;
    buffer_space v lexbuf )

# 2998 "lib/read.ml"

  
# 2999 "lib/read.ml"
  | 4 ->

# 985 "lib/read.mll"
                             
# 985 "lib/read.mll"
                             ( () )

# 3003 "lib/read.ml"

  
# 3004 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_space_rec v lexbuf __ocaml_lex_state

and buffer_object_end v lexbuf =
   __ocaml_lex_buffer_object_end_rec v lexbuf 478
and __ocaml_lex_buffer_object_end_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 988 "lib/read.mll"
             
# 988 "lib/read.mll"
             (
      Buffer.add_char v.buf '}';
      raise Common.End_of_object )

# 3017 "lib/read.ml"

  
# 3018 "lib/read.ml"
  | 1 ->

# 991 "lib/read.mll"
             
# 991 "lib/read.mll"
             ( () )

# 3022 "lib/read.ml"

  
# 3023 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_object_end_rec v lexbuf __ocaml_lex_state

and buffer_object_sep v lexbuf =
   __ocaml_lex_buffer_object_sep_rec v lexbuf 480
and __ocaml_lex_buffer_object_sep_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 994 "lib/read.mll"
             
# 994 "lib/read.mll"
             ( Buffer.add_char v.buf ',' )

# 3034 "lib/read.ml"

  
# 3035 "lib/read.ml"
  | 1 ->

# 995 "lib/read.mll"
             
# 995 "lib/read.mll"
             ( Buffer.add_char v.buf '}'; raise Common.End_of_object )

# 3039 "lib/read.ml"

  
# 3040 "lib/read.ml"
  | 2 ->

# 996 "lib/read.mll"
             
# 996 "lib/read.mll"
             ( long_error "Expected ',' or '}' but found" v lexbuf )

# 3044 "lib/read.ml"

  
# 3045 "lib/read.ml"
  | 3 ->

# 997 "lib/read.mll"
             
# 997 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 3049 "lib/read.ml"

  
# 3050 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_object_sep_rec v lexbuf __ocaml_lex_state

and buffer_array_end v lexbuf =
   __ocaml_lex_buffer_array_end_rec v lexbuf 485
and __ocaml_lex_buffer_array_end_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 1000 "lib/read.mll"
             
# 1000 "lib/read.mll"
             ( Buffer.add_char v.buf ']'; raise Common.End_of_array )

# 3061 "lib/read.ml"

  
# 3062 "lib/read.ml"
  | 1 ->

# 1001 "lib/read.mll"
             
# 1001 "lib/read.mll"
             ( () )

# 3066 "lib/read.ml"

  
# 3067 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_array_end_rec v lexbuf __ocaml_lex_state

and buffer_array_sep v lexbuf =
   __ocaml_lex_buffer_array_sep_rec v lexbuf 487
and __ocaml_lex_buffer_array_sep_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 1004 "lib/read.mll"
             
# 1004 "lib/read.mll"
             ( Buffer.add_char v.buf ',' )

# 3078 "lib/read.ml"

  
# 3079 "lib/read.ml"
  | 1 ->

# 1005 "lib/read.mll"
             
# 1005 "lib/read.mll"
             ( Buffer.add_char v.buf ']'; raise Common.End_of_array )

# 3083 "lib/read.ml"

  
# 3084 "lib/read.ml"
  | 2 ->

# 1006 "lib/read.mll"
             
# 1006 "lib/read.mll"
             ( long_error "Expected ',' or ']' but found" v lexbuf )

# 3088 "lib/read.ml"

  
# 3089 "lib/read.ml"
  | 3 ->

# 1007 "lib/read.mll"
             
# 1007 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 3093 "lib/read.ml"

  
# 3094 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_array_sep_rec v lexbuf __ocaml_lex_state

and buffer_tuple_end v lexbuf =
   __ocaml_lex_buffer_tuple_end_rec v lexbuf 492
and __ocaml_lex_buffer_tuple_end_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 1010 "lib/read.mll"
             
# 1010 "lib/read.mll"
             (
      Buffer.add_char v.buf ')';
      raise Common.End_of_tuple )

# 3107 "lib/read.ml"

  
# 3108 "lib/read.ml"
  | 1 ->

# 1013 "lib/read.mll"
             
# 1013 "lib/read.mll"
             ( () )

# 3112 "lib/read.ml"

  
# 3113 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_tuple_end_rec v lexbuf __ocaml_lex_state

and buffer_tuple_sep v lexbuf =
   __ocaml_lex_buffer_tuple_sep_rec v lexbuf 494
and __ocaml_lex_buffer_tuple_sep_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 1016 "lib/read.mll"
             
# 1016 "lib/read.mll"
             ( Buffer.add_char v.buf ',' )

# 3124 "lib/read.ml"

  
# 3125 "lib/read.ml"
  | 1 ->

# 1017 "lib/read.mll"
             
# 1017 "lib/read.mll"
             ( Buffer.add_char v.buf ')'; raise Common.End_of_tuple )

# 3129 "lib/read.ml"

  
# 3130 "lib/read.ml"
  | 2 ->

# 1018 "lib/read.mll"
             
# 1018 "lib/read.mll"
             ( long_error "Expected ',' or ')' but found" v lexbuf )

# 3134 "lib/read.ml"

  
# 3135 "lib/read.ml"
  | 3 ->

# 1019 "lib/read.mll"
             
# 1019 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 3139 "lib/read.ml"

  
# 3140 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_tuple_sep_rec v lexbuf __ocaml_lex_state

and buffer_colon v lexbuf =
   __ocaml_lex_buffer_colon_rec v lexbuf 499
and __ocaml_lex_buffer_colon_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 1022 "lib/read.mll"
             
# 1022 "lib/read.mll"
             ( Buffer.add_char v.buf ':' )

# 3151 "lib/read.ml"

  
# 3152 "lib/read.ml"
  | 1 ->

# 1023 "lib/read.mll"
             
# 1023 "lib/read.mll"
             ( long_error "Expected ':' but found" v lexbuf )

# 3156 "lib/read.ml"

  
# 3157 "lib/read.ml"
  | 2 ->

# 1024 "lib/read.mll"
             
# 1024 "lib/read.mll"
             ( custom_error "Unexpected end of input" v lexbuf )

# 3161 "lib/read.ml"

  
# 3162 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_colon_rec v lexbuf __ocaml_lex_state

and buffer_gt v lexbuf =
   __ocaml_lex_buffer_gt_rec v lexbuf 503
and __ocaml_lex_buffer_gt_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 1027 "lib/read.mll"
         
# 1027 "lib/read.mll"
         ( Buffer.add_char v.buf '>' )

# 3173 "lib/read.ml"

  
# 3174 "lib/read.ml"
  | 1 ->

# 1028 "lib/read.mll"
         
# 1028 "lib/read.mll"
         ( long_error "Expected '>' but found" v lexbuf )

# 3178 "lib/read.ml"

  
# 3179 "lib/read.ml"
  | 2 ->

# 1029 "lib/read.mll"
         
# 1029 "lib/read.mll"
         ( custom_error "Unexpected end of input" v lexbuf )

# 3183 "lib/read.ml"

  
# 3184 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_buffer_gt_rec v lexbuf __ocaml_lex_state

and finish_buffer_comment v lexbuf =
   __ocaml_lex_finish_buffer_comment_rec v lexbuf 507
and __ocaml_lex_finish_buffer_comment_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->

# 1032 "lib/read.mll"
         
# 1032 "lib/read.mll"
         ( Buffer.add_string v.buf "*/" )

# 3195 "lib/read.ml"

  
# 3196 "lib/read.ml"
  | 1 ->

# 1033 "lib/read.mll"
         
# 1033 "lib/read.mll"
         ( long_error "Unterminated comment" v lexbuf )

# 3200 "lib/read.ml"

  
# 3201 "lib/read.ml"
  | 2 ->

# 1034 "lib/read.mll"
         
# 1034 "lib/read.mll"
         ( Buffer.add_char v.buf '\n';
           newline v lexbuf;
           finish_buffer_comment v lexbuf )

# 3207 "lib/read.ml"

  
# 3208 "lib/read.ml"
  | 3 ->

# 1037 "lib/read.mll"
         
# 1037 "lib/read.mll"
         ( add_lexeme v.buf lexbuf; finish_buffer_comment v lexbuf )

# 3212 "lib/read.ml"

  
# 3213 "lib/read.ml"
  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_finish_buffer_comment_rec v lexbuf __ocaml_lex_state

;;


# 1039 "lib/read.mll"
 
  
# 1040 "lib/read.mll"
  let _ = (read_json : lexer_state -> Lexing.lexbuf -> t)

  let read_t = read_json

  let read_int8 v lexbuf =
    let n = read_int v lexbuf in
    if n < 0 || n > 255 then
      lexer_error "Int8 overflow" v lexbuf
    else
      char_of_int n

  let read_list read_cell v lexbuf =
    List.rev (read_list_rev read_cell v lexbuf)

  let array_of_rev_list l =
    match l with
        [] -> [| |]
      | x :: tl ->
          let len = List.length l in
          let a = Array.make len x in
          let r = ref tl in
          for i = len - 2 downto 0 do
            a.(i) <- List.hd !r;
            r := List.tl !r
          done;
          a

  let read_array read_cell v lexbuf =
    let l = read_list_rev read_cell v lexbuf in
    array_of_rev_list l

  (* Read a JSON object, reading the keys into OCaml strings
     (provided for backward compatibility) *)
  let read_fields read_field init_acc v =
    read_abstract_fields read_ident read_field init_acc v

  let finish v lexbuf =
    read_space v lexbuf;
    if not (read_eof lexbuf) then
      long_error "Junk after end of JSON value:" v lexbuf

  let init_lexer = Common.init_lexer

  let from_lexbuf v ?(stream = false) lexbuf =
    read_space v lexbuf;

    let x =
      if read_eof lexbuf then
        raise Common.End_of_input
      else
        read_json v lexbuf
    in

    if not stream then
      finish v lexbuf;

    x


  let from_string ?buf ?fname ?lnum s =
    try
      let lexbuf = Lexing.from_string s in
      let v = init_lexer ?buf ?fname ?lnum () in
      from_lexbuf v lexbuf
    with Common.End_of_input ->
      Common.json_error "Blank input data"

  let from_channel ?buf ?fname ?lnum ic =
    try
      let lexbuf = Lexing.from_channel ic in
      let v = init_lexer ?buf ?fname ?lnum () in
      from_lexbuf v lexbuf
    with Common.End_of_input ->
      Common.json_error "Blank input data"

  let from_file ?buf ?fname ?lnum file =
    let ic = open_in file in
    try
      let x = from_channel ?buf ?fname ?lnum ic in
      close_in ic;
      x
    with e ->
      close_in_noerr ic;
      raise e

  exception Finally of exn * exn

  let seq_from_lexbuf v ?(fin = fun () -> ()) lexbuf =
    let stream = Some true in
    let rec f () =
      try Seq.Cons (from_lexbuf v ?stream lexbuf, f)
      with
          Common.End_of_input ->
            fin ();
            Seq.Nil
        | e ->
            (try fin () with fin_e -> raise (Finally (e, fin_e)));
            raise e
    in
    f

  let seq_from_string ?buf ?fname ?lnum s =
    let v = init_lexer ?buf ?fname ?lnum () in
    seq_from_lexbuf v (Lexing.from_string s)

  let seq_from_channel ?buf ?fin ?fname ?lnum ic =
    let lexbuf = Lexing.from_channel ic in
    let v = init_lexer ?buf ?fname ?lnum () in
    seq_from_lexbuf v ?fin lexbuf

  let seq_from_file ?buf ?fname ?lnum file =
    let ic = open_in file in
    let fin () = close_in ic in
    let fname =
      match fname with
          None -> Some file
        | x -> x
    in
    let lexbuf = Lexing.from_channel ic in
    let v = init_lexer ?buf ?fname ?lnum () in
    seq_from_lexbuf v ~fin lexbuf

  type json_line = [ `Json of t | `Exn of exn ]

  let lineseq_from_channel
      ?buf ?(fin = fun () -> ()) ?fname ?lnum:(lnum0 = 1) ic =
    let buf =
      match buf with
          None -> Some (Buffer.create 256)
        | Some _ -> buf
    in
    let rec f lnum = fun () ->
      try
        let line = input_line ic in
        Seq.Cons (`Json (from_string ?buf ?fname ~lnum line), f (lnum + 1))
      with
          End_of_file -> fin (); Seq.Nil
        | e -> Seq.Cons (`Exn e, f (lnum + 1))
    in
    f lnum0

  let lineseq_from_file ?buf ?fname ?lnum file =
    let ic = open_in file in
    let fin () = close_in ic in
    let fname =
      match fname with
          None -> Some file
        | x -> x
    in
    lineseq_from_channel ?buf ~fin ?fname ?lnum ic

  let prettify ?std s =
    pretty_to_string ?std (from_string s)

  let compact ?std:_ s =
    to_string (from_string s)


# 3378 "lib/read.ml"

# 19 "basic.cppo.ml"
module Util = struct
# 1 "util.ml"
exception Type_error of string * t

let typeof = function
  | `Assoc _ -> "object"
  | `Bool _ -> "bool"
  | `Float _ -> "float"
  
# 8 "util.ml"
  | `Int _ -> "int"
  
# 10 "util.ml"
  | `List _ -> "array"
  | `Null -> "null"
  | `String _ -> "string"
  | `Intlit _ -> "intlit"
  | `Floatlit _ -> "floatlit"
  
# 18 "util.ml"
  | `Tuple _ -> "tuple"
  | `Variant _ -> "variant"

let typerr msg js = raise (Type_error (msg ^ typeof js, js))

exception Undefined of string * t

let assoc name obj = try List.assoc name obj with Not_found -> `Null

let member name = function
  | `Assoc obj -> assoc name obj
  | js -> typerr ("Can't get member '" ^ name ^ "' of non-object type ") js

let rec path l obj =
  match l with
  | [] -> Some obj
  | key :: l -> (
      match obj with
      | `Assoc assoc -> (
          match List.assoc key assoc with
          | obj -> path l obj
          | exception Not_found -> None)
      | _ -> None)

let index i = function
  | `List l as js ->
      let len = List.length l in
      let wrapped_index = if i < 0 then len + i else i in
      if wrapped_index < 0 || wrapped_index >= len then
        raise (Undefined ("Index " ^ string_of_int i ^ " out of bounds", js))
      else List.nth l wrapped_index
  | js ->
      typerr ("Can't get index " ^ string_of_int i ^ " of non-array type ") js

let map f = function
  | `List l -> `List (List.map f l)
  | js -> typerr "Can't map function over non-array type " js

let to_assoc = function
  | `Assoc obj -> obj
  | js -> typerr "Expected object, got " js

let to_option f = function `Null -> None | x -> Some (f x)
let to_bool = function `Bool b -> b | js -> typerr "Expected bool, got " js

let to_bool_option = function
  | `Bool b -> Some b
  | `Null -> None
  | js -> typerr "Expected bool or null, got " js

let to_number = function
  
# 70 "util.ml"
  | `Int i -> float i
  
# 73 "util.ml"
  | `Float f -> f
  
# 75 "util.ml"
  | js -> typerr "Expected number, got " js

let to_number_option = function
  
# 79 "util.ml"
  | `Int i -> Some (float i)
  
# 82 "util.ml"
  | `Float f -> Some f
  
# 84 "util.ml"
  | `Null -> None
  | js -> typerr "Expected number or null, got " js

let to_float = function
  
# 89 "util.ml"
  | `Float f -> f
  
# 91 "util.ml"
  | js -> typerr "Expected float, got " js

let to_float_option = function
  
# 95 "util.ml"
  | `Float f -> Some f
  
# 97 "util.ml"
  | `Null -> None
  | js -> typerr "Expected float or null, got " js

let to_int = function
  
# 102 "util.ml"
  | `Int i -> i
  
# 104 "util.ml"
  | js -> typerr "Expected int, got " js

let to_int_option = function
  
# 108 "util.ml"
  | `Int i -> Some i
  
# 110 "util.ml"
  | `Null -> None
  | js -> typerr "Expected int or null, got " js

let to_list = function `List l -> l | js -> typerr "Expected array, got " js

let to_string = function
  
# 117 "util.ml"
  | `String s -> s
  
# 119 "util.ml"
  | js -> typerr "Expected string, got " js

let to_string_option = function
  
# 123 "util.ml"
  | `String s -> Some s
  
# 125 "util.ml"
  | `Null -> None
  | js -> typerr "Expected string or null, got " js

let convert_each f = function
  | `List l -> List.map f l
  | js -> typerr "Can't convert each element of non-array type " js

let rec rev_filter_map f acc l =
  match l with
  | [] -> acc
  | x :: tl -> (
      match f x with
      | None -> rev_filter_map f acc tl
      | Some y -> rev_filter_map f (y :: acc) tl)

let filter_map f l = List.rev (rev_filter_map f [] l)

let rec rev_flatten acc l =
  match l with
  | [] -> acc
  | x :: tl -> (
      match x with
      | `List l2 -> rev_flatten (List.rev_append l2 acc) tl
      | _ -> rev_flatten acc tl)

let flatten l = List.rev (rev_flatten [] l)

let filter_index i l =
  filter_map
    (function
      | `List l -> ( try Some (List.nth l i) with _ -> None) | _ -> None)
    l

let filter_list l = filter_map (function `List l -> Some l | _ -> None) l

let filter_member k l =
  filter_map
    (function
      | `Assoc l -> ( try Some (List.assoc k l) with _ -> None) | _ -> None)
    l

let filter_assoc l = filter_map (function `Assoc l -> Some l | _ -> None) l
let filter_bool l = filter_map (function `Bool x -> Some x | _ -> None) l
let filter_int l =
  filter_map (
      function
      
# 172 "util.ml"
      |  `Int x -> Some x
      
# 174 "util.ml"
      | _ -> None
    ) l

let filter_float l =
  filter_map (
    function
      
# 181 "util.ml"
      `Float x -> Some x
      
# 183 "util.ml"
      | _ -> None
  ) l

let filter_number l =
  filter_map (
    function
        
# 190 "util.ml"
        `Int x -> Some (float x)
      
# 193 "util.ml"
      | `Float x -> Some x
      
# 195 "util.ml"
      | _ -> None
  ) l

let filter_string l =
  filter_map (
    function
        
# 202 "util.ml"
        `String x -> Some x
      
# 204 "util.ml"
      | _ -> None
  ) l

let keys o =
  to_assoc o |> List.map (fun (key, _) -> key)

let values o =
  to_assoc o |> List.map (fun (_, value) -> value)

let combine (first : t) (second : t) =
  match (first, second) with
  | `Assoc a, `Assoc b -> (`Assoc (a @ b) : t)
  | a, b -> raise (Invalid_argument "Expected two objects, check inputs")
# 21 "basic.cppo.ml"
end

