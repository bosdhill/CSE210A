load ../../harness

@test "e19aa374e6cb" {
  check 'while (¬(y    *  x     =    2 *   0))      do UX:=4     +3 ' '⇒ skip, {}'
}
