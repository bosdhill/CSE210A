load ../../harness

@test "901da2a77303" {
  check 'if (¬(2   - -4    =    x  *     0))  then   
skip  else skip' '⇒ skip, {}'
}
