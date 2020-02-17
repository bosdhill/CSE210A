load ../../harness

@test "fb11e6100302" {
  check 'x    :=     y    +y  ;  z   := 1  ' '⇒ skip; z := 1, {x → 0}
⇒ z := 1, {x → 0}
⇒ skip, {x → 0, z → 1}'
}
