load ../../harness

@test "c3029bdf8c93" {
  check 'Y     :=     w    + y;z   :=   0*  2    ' '⇒ skip; z := (0*2), {Y → 0}
⇒ z := (0*2), {Y → 0}
⇒ skip, {Y → 0, z → 0}'
}
