load ../../harness

@test "1deca4449fe2" {
  check 'if (z  -3    <z  -    2)     then  skip else  
z  := -1*4     ' '⇒ skip, {}'
}
