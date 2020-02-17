load ../../harness

@test "b0260088eaae" {
  check 'if (false    ∨ 1   *x<     y+    4) then     
y    :=     -1-   XT  else  skip  ' '⇒ y := (-1-XT), {}
⇒ skip, {y → -1}'
}
