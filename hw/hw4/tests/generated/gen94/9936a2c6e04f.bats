load ../../harness

@test "9936a2c6e04f" {
  check 'if true      then 
 skip   else 
skip     ' '⇒ skip, {}'
}
