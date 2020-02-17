load ../../harness

@test "642c31d879d5" {
  check 'x     := 3    + W  ' '⇒ skip, {x → 3}'
}
