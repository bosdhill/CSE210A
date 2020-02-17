load ../../harness

@test "2ecc51edb366" {
  check 'while false   ∧  true   do  z  :=    y   -2 ' '⇒ skip, {}'
}
