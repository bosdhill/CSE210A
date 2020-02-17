load ../../harness

@test "430d2b858f60" {
  check 'while x    *   f   =    3     +     1    ∧   true do skip  ' '⇒ skip, {}'
}
