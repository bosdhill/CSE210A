load ../../harness

@test "a54c5de577b7" {
  check 'if (z   *     y    <    y  -0    ∧  0  +    -4     =-1)   then 

 y   :=   1     *  -4  else T  := 2 *  -3  ' '⇒ T := (2*-3), {}
⇒ skip, {T → -6}'
}
