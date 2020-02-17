load ../../harness

@test "843836951471" {
  check 'skip;W :=     0-  -1 ' '⇒ W := (0--1), {}
⇒ skip, {W → 1}'
}
