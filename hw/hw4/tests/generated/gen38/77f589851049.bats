load ../../harness

@test "77f589851049" {
  check 'while -4   -   0     =   0 *   -2     do skip   ' '⇒ skip, {}'
}
