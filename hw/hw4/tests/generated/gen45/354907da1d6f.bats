load ../../harness

@test "354907da1d6f" {
  check 'x     :=4     *     y ' '⇒ skip, {x → 0}'
}
