load ../../harness

@test "7c316ba71f7d" {
  check 'skip   ;x:=    -4 + -1  ' '⇒ x := (-4+-1), {}
⇒ skip, {x → -5}'
}
