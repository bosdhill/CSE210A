load ../../harness

@test "842d904186ff" {
  check 'while 0     -z     =-1 +    -4   do skip  ' '⇒ skip, {}'
}
