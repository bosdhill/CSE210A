load ../../harness

@test "41e8b74bea7b" {
  check 'skip     ;  x   :=     y   +   3    ' '⇒ x := (y+3), {}
⇒ skip, {x → 3}'
}
