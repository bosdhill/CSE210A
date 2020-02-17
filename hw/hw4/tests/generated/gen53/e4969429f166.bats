load ../../harness

@test "e4969429f166" {
  check 'while 0     + x  =     1     ∨   false      do skip' '⇒ skip, {}'
}
