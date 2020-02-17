load ../../harness

@test "25ab1ca2ebd2" {
  check 'while (¬(-4     +    y  < x     - 1))    do skip' '⇒ skip, {}'
}
