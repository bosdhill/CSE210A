load ../../harness

@test "8243d3d5aee6" {
  check 'while 1  -  -3    < -3  *y   ∧    true      do skip  ' '⇒ skip, {}'
}
