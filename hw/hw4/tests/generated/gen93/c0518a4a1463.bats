load ../../harness

@test "c0518a4a1463" {
  check 'skip ;x    := dQ     +x   ' '⇒ x := (dQ+x), {}
⇒ skip, {x → 0}'
}
