load ../../harness

@test "f459fecb04e3" {
  check 'x   :=    3 * y    ' '⇒ skip, {x → 0}'
}
