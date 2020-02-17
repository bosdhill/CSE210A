load ../../harness

@test "58938669d87e" {
  check 'if (4*     x<    -3 * 1  ∧    3   <4) then 
z  :=     3     *     x      else  
skip  ' '⇒ skip, {}'
}
