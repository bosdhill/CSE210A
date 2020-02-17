load ../../harness

@test "ff88ccb0291a" {
  check 'while 0-z<   2    ∧   false do    x  :=2  -   rb  ' '⇒ skip, {}'
}
