load ../../harness

@test "3e25b99b0869" {
  check 'while false∧true      do   z :=  xX    *z   * z ' '⇒ skip, {}'
}
