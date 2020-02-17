load ../../harness

@test "a4f12e99ab5e" {
  check 'if (¬true)     then z:=   y     -4 else   

z :=   -4-  -4     ' '⇒ z := (-4--4), {}
⇒ skip, {z → 0}'
}
