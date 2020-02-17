load ../../harness

@test "e7cd91a12f72" {
  check 'while 4 +   -1 =    -2 -4 ∧  false do  y:=  4     *     y   ' '⇒ skip, {}'
}
