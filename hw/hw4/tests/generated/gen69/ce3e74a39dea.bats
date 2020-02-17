load ../../harness

@test "ce3e74a39dea" {
  check 'while -2   *     -1 <1  *3  *   -1     do  y  :=    -3-  -1 ' '⇒ skip, {}'
}
