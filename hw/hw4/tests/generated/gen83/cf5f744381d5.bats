load ../../harness

@test "cf5f744381d5" {
  check 'while 3    + 1   <-1  -    0    ∧   false do   skip  ' '⇒ skip, {}'
}
