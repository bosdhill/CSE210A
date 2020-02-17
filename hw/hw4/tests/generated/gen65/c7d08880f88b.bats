load ../../harness

@test "c7d08880f88b" {
  check 'skip ;M     :=   x     -    -1    ' '⇒ M := (x--1), {}
⇒ skip, {M → 1}'
}
