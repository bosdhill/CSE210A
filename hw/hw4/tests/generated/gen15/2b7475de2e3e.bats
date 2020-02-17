load ../../harness

@test "2b7475de2e3e" {
  check 'skip; z   :=     -3 -z     ' '⇒ z := (-3-z), {}
⇒ skip, {z → -3}'
}
