load ../../harness

@test "6e9cfb255ce4" {
  check 'z:=   -4   +     2   ;x    :=    x     ' '⇒ skip; x := x, {z → -2}
⇒ x := x, {z → -2}
⇒ skip, {x → 0, z → -2}'
}
