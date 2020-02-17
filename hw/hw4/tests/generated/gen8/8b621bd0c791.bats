load ../../harness

@test "8b621bd0c791" {
  check 'while (¬true)     do 
  z :=   z     -  -4    ' '⇒ skip, {}'
}
