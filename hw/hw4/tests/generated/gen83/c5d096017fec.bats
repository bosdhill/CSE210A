load ../../harness

@test "c5d096017fec" {
  check 'if (true ∧     1    * 4   =  3 -  x)  then    skip else 
skip  ' '⇒ skip, {}'
}
