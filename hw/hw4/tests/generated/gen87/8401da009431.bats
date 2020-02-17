load ../../harness

@test "8401da009431" {
  check 'if (false ∨  -2    -(h -   0)     <    x     +    y) then   y    := x   *z else 
  skip ' '⇒ y := (x*z), {}
⇒ skip, {y → 0}'
}
