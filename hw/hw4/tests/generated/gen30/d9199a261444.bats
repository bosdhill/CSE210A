load ../../harness

@test "d9199a261444" {
  check 'x:=  -3*-1   ' '⇒ skip, {x → 3}'
}
