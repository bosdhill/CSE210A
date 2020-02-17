load ../../harness

@test "152ec9568ce3" {
  check 'while 1   *  1    =  2+ 3  ∧-3  *-1   <  z --4      do   z :=  2    -     -1 ' '⇒ skip, {}'
}
