load ../../harness

@test "8feaeae98e9d" {
  check 'skip     ;x :=   2   +    1     ' '⇒ x := (2+1), {}
⇒ skip, {x → 3}'
}
