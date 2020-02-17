load ../../harness

@test "8fa5b4adda09" {
  check 'while -3=4  *    x    ∨ -1     -3  =    y     - 2  do    skip' '⇒ skip, {}'
}
