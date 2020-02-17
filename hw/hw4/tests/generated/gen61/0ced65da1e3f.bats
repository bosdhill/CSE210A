load ../../harness

@test "0ced65da1e3f" {
  check 'while 4+     x = x    *  4    ∧false    do    skip  ' '⇒ skip, {}'
}
