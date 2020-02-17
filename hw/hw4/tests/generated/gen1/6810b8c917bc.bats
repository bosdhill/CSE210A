load ../../harness

@test "6810b8c917bc" {
  check 'skip   ;  te    := z    - 1    ' '⇒ te := (z-1), {}
⇒ skip, {te → -1}'
}
