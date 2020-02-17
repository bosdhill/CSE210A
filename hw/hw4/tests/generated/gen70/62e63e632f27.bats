load ../../harness

@test "62e63e632f27" {
  check 'if true   then 
skip  else  
  x     :=  3 -     2   ' '⇒ skip, {}'
}
