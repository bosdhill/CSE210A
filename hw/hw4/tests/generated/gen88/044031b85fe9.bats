load ../../harness

@test "044031b85fe9" {
  check 'y   :=  -3-x  ' '⇒ skip, {y → -3}'
}
