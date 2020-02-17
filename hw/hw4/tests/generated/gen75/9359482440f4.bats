load ../../harness

@test "9359482440f4" {
  check 'y     :=  y' '⇒ skip, {y → 0}'
}
