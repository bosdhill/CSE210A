load ../../harness

@test "65d82fe5bd17" {
  check 'while true  ∧  x +    y  <  z  *z    do skip    ' '⇒ skip, {}'
}
