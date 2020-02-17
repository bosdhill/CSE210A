load ../../harness

@test "1b18d8b479a2" {
  check 'y    :=    x +     z   ' '⇒ skip, {y → 0}'
}
