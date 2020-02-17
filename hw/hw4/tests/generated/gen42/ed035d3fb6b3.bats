load ../../harness

@test "ed035d3fb6b3" {
  check 'if (-3   * z =   z   -    F   ∨     true)   then 

z    :=   -3     +  -3 else  skip  ' '⇒ z := (-3+-3), {}
⇒ skip, {z → -6}'
}
