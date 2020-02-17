load ../../harness

@test "ab2ff61222c1" {
  check 'y  :=     B- 4  ; z :=     -2-     x    ' '⇒ skip; z := (-2-x), {y → -4}
⇒ z := (-2-x), {y → -4}
⇒ skip, {y → -4, z → -2}'
}
