load ../../harness

@test "9a77eaed9376" {
  check 'if (Xa   -z  <  -4    *-4     ∨   false) then   z  :=y   +    x      else 
skip' '⇒ z := (y+x), {}
⇒ skip, {z → 0}'
}
