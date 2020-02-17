load ../../harness

@test "42b6c03b4132" {
  check 'while false     ∧     0    +   x   = z     +  4  do z  :=-2-   -2  ' '⇒ skip, {}'
}
