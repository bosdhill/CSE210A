load ../../harness

@test "2a748f970863" {
  check 'y   :=  z - 0     ;z :=     4  *y   ' '⇒ skip; z := (4*y), {y → 0}
⇒ z := (4*y), {y → 0}
⇒ skip, {y → 0, z → 0}'
}
