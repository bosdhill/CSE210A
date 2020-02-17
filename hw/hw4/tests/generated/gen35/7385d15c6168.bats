load ../../harness

@test "7385d15c6168" {
  check 'z := x  -q8  ;  y  :=     x-     -4  ' '⇒ skip; y := (x--4), {z → 0}
⇒ y := (x--4), {z → 0}
⇒ skip, {y → 4, z → 0}'
}
