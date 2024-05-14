if [ -t 0 ]; then
  test -r /home/runner/DM4/.opam/opam-init/complete.sh && . /home/runner/DM4/.opam/opam-init/complete.sh > /dev/null 2> /dev/null || true

  test -r /home/runner/DM4/.opam/opam-init/env_hook.sh && . /home/runner/DM4/.opam/opam-init/env_hook.sh > /dev/null 2> /dev/null || true
fi

test -r /home/runner/DM4/.opam/opam-init/variables.sh && . /home/runner/DM4/.opam/opam-init/variables.sh > /dev/null 2> /dev/null || true
