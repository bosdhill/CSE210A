load ../../harness

@test "d0f77bb1713f" {
  check 'while -1    +  x=3 *y  ∧     true  do skip     ' '⇒ skip, {}'
}
