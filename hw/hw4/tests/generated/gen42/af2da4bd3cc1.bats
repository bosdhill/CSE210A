load ../../harness

@test "af2da4bd3cc1" {
  check 'if (2   -  0<y   +y    ∧  -2  -  -3     < 0 *   z) then skip   else 
z     :=   -2     +    z' '⇒ z := (-2+z), {}
⇒ skip, {z → -2}'
}
