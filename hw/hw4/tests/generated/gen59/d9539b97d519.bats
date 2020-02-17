load ../../harness

@test "d9539b97d519" {
  check 'while (¬true)   do 
skip' '⇒ skip, {}'
}
