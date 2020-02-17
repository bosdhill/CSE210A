load ../../harness

@test "e46a94de2852" {
  check 'while false ∧z -    4     = -1   *    x    do   z    :=-3 *   2 ' '⇒ skip, {}'
}
