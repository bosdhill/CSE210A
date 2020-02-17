load ../../harness

@test "3876a5cf6185" {
  check 'if (1    -  z    = -3 * y  ∨    x    = -3 -  -2)    then     x :=     2 -  4  else 
z   :=  0   -3' '⇒ z := (0-3), {}
⇒ skip, {z → -3}'
}
