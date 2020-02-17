load ../../harness

@test "35e9e000f2d4" {
  check 'skip    ;  y     :=  f   + z2    ' '⇒ y := (f+z2), {}
⇒ skip, {y → 0}'
}
