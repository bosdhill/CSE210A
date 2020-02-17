load ../../harness

@test "08cbc765dfa5" {
  check 'while false    ∧  false     do   skip     ' '⇒ skip, {}'
}
