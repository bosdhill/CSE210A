load ../../harness

@test "ba391c0bf90c" {
  check 'while 1  +1  <y   +  z∧    -4     *     1     =4     *     2  do  


{y   :=     4    *  z   ;  
  skip}' '⇒ skip, {}'
}
