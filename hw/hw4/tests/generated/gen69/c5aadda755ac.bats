load ../../harness

@test "c5aadda755ac" {
  check 'if true   then 
skip else   
  x:=   -2 +   y    ' '⇒ skip, {}'
}
