load ../../harness

@test "d4cf9450b4f1" {
  check 'while (¬true)    do 
skip   ' '⇒ skip, {}'
}
