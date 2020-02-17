load ../../harness

@test "27693f29fec4" {
  check 'while z    -     -2  < 2   -     z     ∧-2   *     x     =     1    -z   do x   :=   z   -     z   ' '⇒ skip, {}'
}
