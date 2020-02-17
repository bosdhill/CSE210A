load ../../harness

@test "c91a789b9bfb" {
  check 'while x    -    z    =     2   -z do     skip  ' '⇒ skip, {}'
}
