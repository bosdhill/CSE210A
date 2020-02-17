load ../../harness

@test "f94418a8a6e9" {
  check 'if (true  ∨ -1 *    -2    =    -4)      then 
 x    :=   -3     - x   else x:= 4 +    0   ' '⇒ x := (-3-x), {}
⇒ skip, {x → -3}'
}
