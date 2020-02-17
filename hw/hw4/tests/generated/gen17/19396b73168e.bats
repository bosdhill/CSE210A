load ../../harness

@test "19396b73168e" {
  check 'while false∧-4     *-4   = x     *  y      do skip     ' '⇒ skip, {}'
}
