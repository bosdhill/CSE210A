load ../../harness

@test "04238bcb343a" {
  check 'skip     ;x :=   y  *  3    ' '⇒ x := (y*3), {}
⇒ skip, {x → 0}'
}
