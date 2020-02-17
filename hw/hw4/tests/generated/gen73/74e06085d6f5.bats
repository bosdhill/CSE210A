load ../../harness

@test "74e06085d6f5" {
  check 'while false     ∨   4    +   3=    -3    do   skip    ' '⇒ skip, {}'
}
