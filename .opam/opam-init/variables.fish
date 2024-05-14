# Prefix of the current opam switch
set -gx OPAM_SWITCH_PREFIX '/home/runner/DM4/.opam/default';
# Updated by package ocaml-base-compiler
set -gx CAML_LD_LIBRARY_PATH '/home/runner/DM4/.opam/default/lib/stublibs';
# Updated by package ocaml
set -gx CAML_LD_LIBRARY_PATH '/home/runner/DM4/.opam/default/lib/ocaml/stublibs:/home/runner/DM4/.opam/default/lib/ocaml';
# Updated by package ocaml
set -gx CAML_LD_LIBRARY_PATH '/home/runner/DM4/.opam/default/lib/stublibs':"$CAML_LD_LIBRARY_PATH";
# Updated by package ocaml
set -gx OCAML_TOPLEVEL_PATH '/home/runner/DM4/.opam/default/lib/toplevel';
# Current opam switch man dir
if [ (count $MANPATH) -gt 0 ]; set -gx MANPATH $MANPATH '/home/runner/DM4/.opam/default/man'; end;
# Binary dir for opam switch default
set -gx PATH '/home/runner/DM4/.opam/default/bin' $PATH;
