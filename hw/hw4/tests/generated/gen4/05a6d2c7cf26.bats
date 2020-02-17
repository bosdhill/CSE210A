load ../../harness

@test "05a6d2c7cf26" {
  check 'z := z  *     0   ;


P:=y     - 1' '⇒ skip; P := (y-1), {z → 0}
⇒ P := (y-1), {z → 0}
⇒ skip, {P → -1, z → 0}'
}
