load ../../harness

@test "6894f3fffc13" {
  check 'while (¬true)      do  skip     ' '⇒ skip, {}'
}
