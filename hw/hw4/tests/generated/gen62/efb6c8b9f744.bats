load ../../harness

@test "efb6c8b9f744" {
  check 'z  :=    -2     *     E ; y    :=    x   *    (0  -    -1)' '⇒ skip; y := (x*(0--1)), {z → 0}
⇒ y := (x*(0--1)), {z → 0}
⇒ skip, {y → 0, z → 0}'
}
