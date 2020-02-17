load ../../harness

@test "a8f0d2ece622" {
  check 'while (¬true)    do 
 x  :=   y     *     0' '⇒ skip, {}'
}
