load ../../harness

@test "8ef3c57090ff" {
  check 'if (¬(-2    +  -1  <  2*  y)) then    skip else skip  ' '⇒ skip, {}'
}
