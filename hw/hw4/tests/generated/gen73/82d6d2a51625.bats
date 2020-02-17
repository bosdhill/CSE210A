load ../../harness

@test "82d6d2a51625" {
  check 'while false do skip     ' '⇒ skip, {}'
}
