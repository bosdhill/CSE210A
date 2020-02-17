load ../../harness

@test "7a7578ba4c3f" {
  check 'x     :=     -4    * uf   ;  y  :=  wp   +  0     ' '⇒ skip; y := (wp+0), {x → 0}
⇒ y := (wp+0), {x → 0}
⇒ skip, {x → 0, y → 0}'
}
