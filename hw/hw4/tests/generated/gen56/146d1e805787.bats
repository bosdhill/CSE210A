load ../../harness

@test "146d1e805787" {
  check 'if (¬(DK    + x  <    y     +    -4))    then  
skip    else     z   :=     -4    +x' '⇒ skip, {}'
}
