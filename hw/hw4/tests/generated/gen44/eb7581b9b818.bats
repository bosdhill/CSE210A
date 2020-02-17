load ../../harness

@test "eb7581b9b818" {
  check 'skip ;  y   :=1  +  -1     ' '⇒ y := (1+-1), {}
⇒ skip, {y → 0}'
}
