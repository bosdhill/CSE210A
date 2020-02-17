load ../../harness

@test "d81cccda4e72" {
  check 'while (¬true)     do  skip   ' '⇒ skip, {}'
}
