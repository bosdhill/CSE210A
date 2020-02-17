load ../../harness

@test "2dde36b7d211" {
  check 'if (2  +   y     <    4     *z  ∨     true)    then 
skip  else  
  y :=   Mt    *    4    ' '⇒ skip, {}'
}
