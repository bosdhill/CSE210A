load ../../harness

@test "c352ac8cf6c6" {
  check 'while -1 + 3   <  2     +     -1 ∧   false     do      o0:=     1    *-4     ' '⇒ skip, {}'
}
