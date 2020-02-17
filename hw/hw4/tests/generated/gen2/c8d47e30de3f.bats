load ../../harness

@test "c8d47e30de3f" {
  check 'if (¬false)   then  y :=     v  +  0      else U1   :=  x    ' '⇒ y := (v+0), {}
⇒ skip, {y → 0}'
}
