load ../../harness

@test "459484bfa215" {
  check 'if (-3   *4    < lG+z  ∧ x *   -1  <    3  + 3)      then   skip     else  skip ' '⇒ skip, {}'
}
