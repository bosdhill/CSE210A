load ../../harness

@test "9d3a42d76076" {
  check 'skip    ;x   :=     y -     y     ' '⇒ x := (y-y), {}
⇒ skip, {x → 0}'
}
