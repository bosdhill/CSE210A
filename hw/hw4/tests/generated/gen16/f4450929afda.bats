load ../../harness

@test "f4450929afda" {
  check 'while (¬(1  + -3<     3- z))  do skip   ' '⇒ skip, {}'
}
