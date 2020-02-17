load ../../harness

@test "cd67e4c79c6f" {
  check 'if (-2     +     z     < x *     y ∧     -3   +  -1    <    1   * 0) then 
z  :=     z  -     z     else  skip ' '⇒ z := (z-z), {}
⇒ skip, {z → 0}'
}
