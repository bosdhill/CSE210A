load ../../harness

@test "16692a2ae29f" {
  check 'while 3   *  2    <    AA    *    3  ∨  -2   *    -3=   3 +     -3      do   f3     :=     x    *     y' '⇒ skip, {}'
}
