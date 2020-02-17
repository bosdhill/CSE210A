load ../../harness

@test "e5da5ff96141" {
  check 'while false     ∨0   + 2   =  0     -  y    do    skip' '⇒ skip, {}'
}
