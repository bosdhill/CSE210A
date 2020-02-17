load ../../harness

@test "34d5dd579427" {
  check 'if (y   * z=   4  *  -2 ∧   false)  then 
  skip      else  x     :=   x-     1  ' '⇒ x := (x-1), {}
⇒ skip, {x → -1}'
}
