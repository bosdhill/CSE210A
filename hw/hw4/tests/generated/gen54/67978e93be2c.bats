load ../../harness

@test "67978e93be2c" {
  check 'while -2   =    1-    z      do  z   :=     1  *  x   ' '⇒ skip, {}'
}
