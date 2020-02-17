load ../../harness

@test "6ff7742ca30d" {
  check 'skip;x     :=     Z   -  x ' '⇒ x := (Z-x), {}
⇒ skip, {x → 0}'
}
