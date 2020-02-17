load ../../harness

@test "4897e8113ed7" {
  check 'x :=  -2     *  1    ;x:=     x   +     3 ' '⇒ skip; x := (x+3), {x → -2}
⇒ x := (x+3), {x → -2}
⇒ skip, {x → 1}'
}
