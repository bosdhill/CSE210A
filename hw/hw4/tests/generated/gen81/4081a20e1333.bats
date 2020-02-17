load ../../harness

@test "4081a20e1333" {
  check 'z := y   *    y' '⇒ skip, {z → 0}'
}
