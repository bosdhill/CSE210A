load ../../harness

@test "aa05cf991368" {
  check 'while false   ∧  4     +  -1<    3*  x      do  x     :=z    - y' '⇒ skip, {}'
}
