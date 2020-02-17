load ../../harness

@test "2980045fc02e" {
  check 'if (3 +4   *2<y  -  y   ∨false) then   skip    else 

 y    :=0    + 0    ' '⇒ y := (0+0), {}
⇒ skip, {y → 0}'
}
