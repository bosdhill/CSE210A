load ../../harness

@test "f57ac63e4c24" {
  check 'if (P    *    3  <   z     *  y ∨     J +    x <3   --3)  then 
 z  :=     x  +     -4   else      skip     ' '⇒ z := (x+-4), {}
⇒ skip, {z → -4}'
}
