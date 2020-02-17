load ../../harness

@test "4866e0b8cb6d" {
  check 'while (¬true)   do 
BS := H *   x ' '⇒ skip, {}'
}
