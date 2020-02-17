load ../../harness

@test "0be1fa6b798e" {
  check 'y     := z   +     -2  ' '⇒ skip, {y → -2}'
}
