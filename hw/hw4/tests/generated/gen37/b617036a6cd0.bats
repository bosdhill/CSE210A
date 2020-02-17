load ../../harness

@test "b617036a6cd0" {
  check 'z     :=     1    *    2     ' '⇒ skip, {z → 2}'
}
