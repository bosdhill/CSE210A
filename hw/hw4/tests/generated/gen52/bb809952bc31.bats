load ../../harness

@test "bb809952bc31" {
  check 'while false     do  skip  ' '⇒ skip, {}'
}
