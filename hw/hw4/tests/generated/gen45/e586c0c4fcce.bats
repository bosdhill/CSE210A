load ../../harness

@test "e586c0c4fcce" {
  check 'while (¬true)   do  skip' '⇒ skip, {}'
}
