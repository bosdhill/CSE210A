load ../../harness

@test "dda59c677f80" {
  check 'if (¬(1     *z  <   z)) then  
skip   else 
  skip' '⇒ skip, {}'
}
