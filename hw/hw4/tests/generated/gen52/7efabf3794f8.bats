load ../../harness

@test "7efabf3794f8" {
  check 'H :=    z    +    2     ;    x  :=0*    2     ' '⇒ skip; x := (0*2), {H → 2}
⇒ x := (0*2), {H → 2}
⇒ skip, {H → 2, x → 0}'
}
