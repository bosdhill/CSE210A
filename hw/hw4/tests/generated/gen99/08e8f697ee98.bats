load ../../harness

@test "08e8f697ee98" {
  check 'while (¬true)   do     skip' '⇒ skip, {}'
}
