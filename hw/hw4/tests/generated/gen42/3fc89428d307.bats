load ../../harness

@test "3fc89428d307" {
  check 'while false      do skip  ' '⇒ skip, {}'
}
