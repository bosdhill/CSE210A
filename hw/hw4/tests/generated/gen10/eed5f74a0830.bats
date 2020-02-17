load ../../harness

@test "eed5f74a0830" {
  check 'if (0   +  2   <0 -     0   ∨     false)      then   x     := -3 +   d     else 
z:=  y    -  y  ' '⇒ z := (y-y), {}
⇒ skip, {z → 0}'
}
