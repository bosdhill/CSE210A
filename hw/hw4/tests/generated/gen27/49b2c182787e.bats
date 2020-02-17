load ../../harness

@test "49b2c182787e" {
  check 'while (¬true)  do  skip' '⇒ skip, {}'
}
