load ../../harness

@test "1465468b7f3a" {
  check 'skip   ; x := 1 -     Vf ' '⇒ x := (1-Vf), {}
⇒ skip, {x → 1}'
}
