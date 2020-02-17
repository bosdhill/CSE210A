load ../../harness

@test "0501a7927072" {
  check 'while false  ∧     2     *     2     < 3     -     3    do  y     :=x    ' '⇒ skip, {}'
}
