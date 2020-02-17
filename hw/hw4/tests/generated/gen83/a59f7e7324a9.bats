load ../../harness

@test "a59f7e7324a9" {
  check 'while false     ∨ y     +2    =     2 +     3    do skip    ' '⇒ skip, {}'
}
