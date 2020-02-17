load ../../harness

@test "38740cacf357" {
  check 'x   :=   -2    -F    ' '⇒ skip, {x → -2}'
}
