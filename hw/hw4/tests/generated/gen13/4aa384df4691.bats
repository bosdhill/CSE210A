load ../../harness

@test "4aa384df4691" {
  check 'while true    ∧     x    +     1  <-1     +   y     do  skip' '⇒ skip, {}'
}
