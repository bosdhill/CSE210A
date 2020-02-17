load ../../harness

@test "62b8da163fc4" {
  check 'if (¬true)   then  skip  else  
skip   ' '⇒ skip, {}'
}
