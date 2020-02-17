load ../../harness

@test "7710d0651ff2" {
  check 'y  := x  -z ' '⇒ skip, {y → 0}'
}
