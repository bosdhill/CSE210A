load ../../harness

@test "d2787a6c50e0" {
  check 'z    := 4     -  z3   ;
skip' '⇒ skip; skip, {z → 4}
⇒ skip, {z → 4}'
}
