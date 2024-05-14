if ( ! ${?OPAMNOENVNOTICE} ) setenv OPAMNOENVNOTICE ""
setenv OPAMNOENVNOTICE true
alias precmd 'eval `opam env --shell=csh --readonly`'
