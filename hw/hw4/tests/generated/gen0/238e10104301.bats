load ../../harness

@test "238e10104301" {
  check 'while -1     *   y = 2  * -2∨false  do  x:=     m  - x     ' '⇒ skip, {}'
}
