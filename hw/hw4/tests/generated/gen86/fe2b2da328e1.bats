load ../../harness

@test "fe2b2da328e1" {
  check 'while y*y  <  z  *-2      do skip' '⇒ skip, {}'
}
