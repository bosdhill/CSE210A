load ../../harness

@test "9082f150a7c4" {
  check 'while 2    -   3   = t0   +  y      do    skip   ' '⇒ skip, {}'
}
