load ../../harness

@test "f330f90264e3" {
  check 'skip ;s    :=3  +    -1    ' '⇒ s := (3+-1), {}
⇒ skip, {s → 2}'
}
