load ../../harness

@test "ae71bb96af05" {
  check 'while false do z     :=-2   +   0     ' '⇒ skip, {}'
}
