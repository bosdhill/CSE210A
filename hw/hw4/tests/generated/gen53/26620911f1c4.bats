load ../../harness

@test "26620911f1c4" {
  check 'while true  ∧    2  *    y  <  3 *   x   do skip   ' '⇒ skip, {}'
}
