load ../../harness

@test "489f5733c41a" {
  check 'while 3   *   z     =   3     * 2  ∨    1<    2   -   2  do x  :=    x     -     z ' '⇒ skip, {}'
}
