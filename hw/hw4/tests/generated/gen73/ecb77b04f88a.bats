load ../../harness

@test "ecb77b04f88a" {
  check 'skip    ;x     :=   x     --4     ' '⇒ x := (x--4), {}
⇒ skip, {x → 4}'
}
