load ../../harness

@test "f5313a8c5ba3" {
  check 'while y + y <  -1   *     z     ∧   false do  x     :=-3     * -2   ' '⇒ skip, {}'
}
