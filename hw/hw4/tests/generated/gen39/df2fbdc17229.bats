load ../../harness

@test "df2fbdc17229" {
  check 'if (true ∧     x    * -1  <  -3     - y)   then  
skip else  

 x   :=     -3 -     2    ' '⇒ x := (-3-2), {}
⇒ skip, {x → -5}'
}
