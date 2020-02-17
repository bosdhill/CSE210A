load ../../harness

@test "c64502af59b7" {
  check 'while 2  -  1     =  y  +   z ∧false do  
 v  :=    4     -  2 ' '⇒ skip, {}'
}
