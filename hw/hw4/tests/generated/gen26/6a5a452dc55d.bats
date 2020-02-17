load ../../harness

@test "6a5a452dc55d" {
  check 'while (¬true)   do 
  skip     ' '⇒ skip, {}'
}
