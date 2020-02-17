load ../../harness

@test "f71740bca762" {
  check 'z  :=y*  2     ;y   :=   0  --2    ' '⇒ skip; y := (0--2), {z → 0}
⇒ y := (0--2), {z → 0}
⇒ skip, {y → 2, z → 0}'
}
