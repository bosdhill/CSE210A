load ../../harness

@test "751dfc53d5f1" {
  check 'skip   ;x :=   -1*    z    ' '⇒ x := (-1*z), {}
⇒ skip, {x → 0}'
}
