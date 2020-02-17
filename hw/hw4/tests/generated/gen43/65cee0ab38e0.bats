load ../../harness

@test "65cee0ab38e0" {
  check 'if (false   ∨   -2=z +  3)     then 
  skip      else z    := 0     +y    ' '⇒ z := (0+y), {}
⇒ skip, {z → 0}'
}
