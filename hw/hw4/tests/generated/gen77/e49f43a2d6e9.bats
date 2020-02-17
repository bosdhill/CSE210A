load ../../harness

@test "e49f43a2d6e9" {
  check 'while -3  *     2    =1 -   3∧   true   do skip' '⇒ skip, {}'
}
