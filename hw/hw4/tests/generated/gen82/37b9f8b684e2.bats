load ../../harness

@test "37b9f8b684e2" {
  check 'while y -     x     <   y  do  skip     ' '⇒ skip, {}'
}
