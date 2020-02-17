load ../../harness

@test "24f49b35f677" {
  check 'while true  ∧ false do skip    ' '⇒ skip, {}'
}
