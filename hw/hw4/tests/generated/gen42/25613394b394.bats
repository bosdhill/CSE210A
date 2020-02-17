load ../../harness

@test "25613394b394" {
  check 'x  :=   2 - x     ' '⇒ skip, {x → 2}'
}
