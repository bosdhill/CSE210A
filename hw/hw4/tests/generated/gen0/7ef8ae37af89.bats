load ../../harness

@test "7ef8ae37af89" {
  check 'while false      do  mb  :=    -1* -4 ' '⇒ skip, {}'
}
