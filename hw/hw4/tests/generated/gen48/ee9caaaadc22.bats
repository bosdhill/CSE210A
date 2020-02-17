load ../../harness

@test "ee9caaaadc22" {
  check 'while z+ y =2     *     2   ∧ z+1 =u     do skip   ' '⇒ skip, {}'
}
