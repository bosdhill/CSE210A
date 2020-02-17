load ../../harness

@test "939150d17e2d" {
  check 'x    :=  z     *    y' '⇒ skip, {x → 0}'
}
