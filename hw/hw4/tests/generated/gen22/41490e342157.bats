load ../../harness

@test "41490e342157" {
  check 'while (¬true)     do 
skip     ' '⇒ skip, {}'
}
