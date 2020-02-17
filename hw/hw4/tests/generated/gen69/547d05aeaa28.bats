load ../../harness

@test "547d05aeaa28" {
  check 'if (2   *     -2=     x     +z     ∧  4 * z   =  0  -    y)     then 

  ci    :=  y   *    -2      else 
  x     :=x -3  ' '⇒ x := (x-3), {}
⇒ skip, {x → -3}'
}
