load ../../harness

@test "e85f5d993612" {
  check 'while 3  -    x  =     y  +     y     ∨   false      do skip' '⇒ skip, {}'
}
