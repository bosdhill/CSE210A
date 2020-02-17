load ../../harness

@test "601320832b0d" {
  check 'if (true∧ y *    x <   z)     then 
   
x    :=     x   +  x else skip    ' '⇒ skip, {}'
}
