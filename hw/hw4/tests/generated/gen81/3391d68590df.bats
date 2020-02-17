load ../../harness

@test "3391d68590df" {
  check 'while false  ∧  true    do skip    ' '⇒ skip, {}'
}
