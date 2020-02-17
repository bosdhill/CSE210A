load ../../harness

@test "ba2f95794688" {
  check 'while false∧ -4   - -3    =2  do    T    :=-1' '⇒ skip, {}'
}
