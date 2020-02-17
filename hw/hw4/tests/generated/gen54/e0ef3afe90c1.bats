load ../../harness

@test "e0ef3afe90c1" {
  check 'if (true  ∧z  - -4  =    -3 *   x)    then DY:= 0     -   3    else 
 x   :=   -2    - 3     ' '⇒ x := (-2-3), {}
⇒ skip, {x → -5}'
}
