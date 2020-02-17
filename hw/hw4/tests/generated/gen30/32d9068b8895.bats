load ../../harness

@test "32d9068b8895" {
  check 'while false     ∧x  - z   <-2    *1    do skip' '⇒ skip, {}'
}
