load ../../harness

@test "798e6b7bf93a" {
  check 'while 2    +    x=-2    do y     :=    x     *    -2    ' '⇒ skip, {}'
}
