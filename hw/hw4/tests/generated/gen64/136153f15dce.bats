load ../../harness

@test "136153f15dce" {
  check 'y:=     y-z ; z  := 4+  Ct   ' '⇒ skip; z := (4+Ct), {y → 0}
⇒ z := (4+Ct), {y → 0}
⇒ skip, {y → 0, z → 4}'
}
