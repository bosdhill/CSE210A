load ../../harness

@test "e597bdf2f22c" {
  check 'while (¬true)     do 
skip ' '⇒ skip, {}'
}
