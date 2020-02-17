load ../../harness

@test "3cb274e07fda" {
  check 'x    :=x +  3  ' '⇒ skip, {x → 3}'
}
