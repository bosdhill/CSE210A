load ../../harness

@test "e86913fade4f" {
  check 'while (¬true)    do 
 skip' '⇒ skip, {}'
}
