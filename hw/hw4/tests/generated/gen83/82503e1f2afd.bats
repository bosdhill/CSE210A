load ../../harness

@test "82503e1f2afd" {
  check 'x:=     -4*3;skip ' '⇒ skip; skip, {x → -12}
⇒ skip, {x → -12}'
}
