load ../../harness

@test "87e210000c5e" {
  check 'while (¬true)    do  skip' '⇒ skip, {}'
}
