load ../../harness

@test "7202393970d8" {
  check 'while 0   +    -3  <    -3    +   0 ∧ -3   +4  =   2 *     3   do skip   ' '⇒ skip, {}'
}
