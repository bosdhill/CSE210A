load ../../harness

@test "bb7e2d3341ac" {
  check 'while false     ∧  true      do  y     := -3   -     1    ' '⇒ skip, {}'
}
