load ../../harness

@test "ff183083447f" {
  check 'z:=   4   -4   ;y   :=     a+ -1    ' '⇒ skip; y := (a+-1), {z → 0}
⇒ y := (a+-1), {z → 0}
⇒ skip, {y → -1, z → 0}'
}
