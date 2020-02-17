load ../../harness

@test "819e67f6d3bd" {
  check 'while (¬(x-y =y  *    z))    do   skip' '⇒ skip, {}'
}
