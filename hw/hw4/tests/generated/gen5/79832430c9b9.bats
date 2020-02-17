load ../../harness

@test "79832430c9b9" {
  check 'while true  ∧  false   do    skip  ' '⇒ skip, {}'
}
