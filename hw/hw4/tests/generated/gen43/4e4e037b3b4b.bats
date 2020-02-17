load ../../harness

@test "4e4e037b3b4b" {
  check 'skip    ;x :=    z +     y ' '⇒ x := (z+y), {}
⇒ skip, {x → 0}'
}
