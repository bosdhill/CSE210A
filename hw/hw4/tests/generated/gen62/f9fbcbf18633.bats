load ../../harness

@test "f9fbcbf18633" {
  check 'while false     ∧    1   <y     + 3    do skip' '⇒ skip, {}'
}
