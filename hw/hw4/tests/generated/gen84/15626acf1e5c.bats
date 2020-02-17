load ../../harness

@test "15626acf1e5c" {
  check 'w    := x  +   -3;skip ' '⇒ skip; skip, {w → -3}
⇒ skip, {w → -3}'
}
