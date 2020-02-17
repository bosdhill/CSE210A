load ../../harness

@test "32e73d8f0fef" {
  check 'if (0*  -4  <   z     +  y    ∨   0+4    = y     + 4)   then R1:=  2   +  3    else skip    ' '⇒ R1 := (2+3), {}
⇒ skip, {R1 → 5}'
}
