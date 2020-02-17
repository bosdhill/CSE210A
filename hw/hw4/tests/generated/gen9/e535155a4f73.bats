load ../../harness

@test "e535155a4f73" {
  check 'while (¬(x     -   1    <y     -   x))      do 
 
skip' '⇒ skip, {}'
}
