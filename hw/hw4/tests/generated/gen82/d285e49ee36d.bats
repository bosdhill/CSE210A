load ../../harness

@test "d285e49ee36d" {
  check 'x    :=   -1     *   z   ' '⇒ skip, {x → 0}'
}
