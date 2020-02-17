load ../../harness

@test "4642ff6f191e" {
  check 'while (¬(1 +    -3<    2    -     y))      do  skip' '⇒ skip, {}'
}
