load ../../harness

@test "13d174c91931" {
  check 'while x -   4=     0    *z  ∨     false      do skip    ' '⇒ skip, {}'
}
