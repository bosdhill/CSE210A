load ../../harness

@test "fe5ddd9077d3" {
  check 'if (true     ∨false)    then  
   U   :=-4 * 2      else z :=     1   -   y     ' '⇒ U := (-4*2), {}
⇒ skip, {U → -8}'
}
