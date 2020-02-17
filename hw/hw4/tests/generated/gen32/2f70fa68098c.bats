load ../../harness

@test "2f70fa68098c" {
  check 'while (¬(y   +  z   = x     +  x))   do      y :=     -1   +   1   ' '⇒ skip, {}'
}
