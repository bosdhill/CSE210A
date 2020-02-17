load ../../harness

@test "4c40e3863483" {
  check 'while (¬true)     do    skip' '⇒ skip, {}'
}
