load ../../harness

@test "03fb654b2a20" {
  check 'while (¬true)   do  z := -1     --3 ' '⇒ skip, {}'
}
