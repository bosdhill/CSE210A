load ../../harness

@test "ae6fb373fc1f" {
  check 'if (x     - 4    = y  -z)  then   
  y :=4    - -3 else x  :=   -4* 4    ' '⇒ x := (-4*4), {}
⇒ skip, {x → -16}'
}
