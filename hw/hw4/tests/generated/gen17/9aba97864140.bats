load ../../harness

@test "9aba97864140" {
  check 'x   :=   -1   +     -2 ;

 x:=     z     + 2 ' '⇒ skip; x := (z+2), {x → -3}
⇒ x := (z+2), {x → -3}
⇒ skip, {x → 2}'
}
