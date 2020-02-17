load ../../harness

@test "aa4c9186b6a9" {
  check 'while false     ∧  false    do z  :=   z - -3     ' '⇒ skip, {}'
}
