load ../../harness

@test "4e054acfb836" {
  check 'if false  then X6:=     -4    - z     else  y := -4 *    0   ;
  
z  :=0   *     x  ' '⇒ y := (-4*0); z := (0*x), {}
⇒ skip; z := (0*x), {y → 0}
⇒ z := (0*x), {y → 0}
⇒ skip, {y → 0, z → 0}'
}
