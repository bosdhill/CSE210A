load ../../harness

@test "44f909593bfc" {
  check 'x:=     s   +     -2   ' '⇒ skip, {x → -2}'
}
