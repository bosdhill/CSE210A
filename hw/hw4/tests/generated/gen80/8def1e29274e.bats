load ../../harness

@test "8def1e29274e" {
  check 'M    := z + -2    ; 
x    :=  z   +   -3' '⇒ skip; x := (z+-3), {M → -2}
⇒ x := (z+-3), {M → -2}
⇒ skip, {M → -2, x → -3}'
}
