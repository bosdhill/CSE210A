load ../../harness

@test "dce1b7e29dee" {
  check 'if (x  +y  =    3 *     -1    ∨ 0    =x   *   le)    then 

 
 y     :=   -4 *     z     else z  := z     +   0' '⇒ y := (-4*z), {}
⇒ skip, {y → 0}'
}
