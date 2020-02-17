load ../../harness

@test "160a1239d3f0" {
  check 'if (-2-  4    = z -  x   ∧ -3     *1  <  0 - y)      then 
 x     :=    y      else 
   x    := 3-x' '⇒ x := (3-x), {}
⇒ skip, {x → 3}'
}
