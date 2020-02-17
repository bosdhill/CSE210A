load ../../harness

@test "fdecec26eb3f" {
  check 'while x     *   2    <    3     *     x ∨2-2    < z    -     y    do x :=  4    ' '⇒ skip, {}'
}
