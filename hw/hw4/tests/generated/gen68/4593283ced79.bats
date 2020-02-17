load ../../harness

@test "4593283ced79" {
  check 'z     :=   -2     -x' '⇒ skip, {z → -2}'
}
