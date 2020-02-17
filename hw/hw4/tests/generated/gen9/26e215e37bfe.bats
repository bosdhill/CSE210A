load ../../harness

@test "26e215e37bfe" {
  check 'y := z   *  Ec' '⇒ skip, {y → 0}'
}
