load ../../harness

@test "dd8c64f5a0b6" {
  check 'X    :=   -1*     x ; z :=    2   *1   ' '⇒ skip; z := (2*1), {X → 0}
⇒ z := (2*1), {X → 0}
⇒ skip, {X → 0, z → 2}'
}
