load ../../harness

@test "7035597f06cd" {
  check 'while (¬(-2    +0 <   y     +  v))   do skip   ' '⇒ skip, {}'
}
