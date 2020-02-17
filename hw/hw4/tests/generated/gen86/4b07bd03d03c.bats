load ../../harness

@test "4b07bd03d03c" {
  check 'while (¬(x   -   0   =  x     *     -2))      do   skip  ' '⇒ skip, {}'
}
