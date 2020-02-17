load ../../harness

@test "72cc68318047" {
  check 'while (¬true)  do  skip    ' '⇒ skip, {}'
}
