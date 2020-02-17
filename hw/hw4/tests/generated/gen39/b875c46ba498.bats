load ../../harness

@test "b875c46ba498" {
  check 'x  := 2 -    -2; x:=y   +     3 ' '⇒ skip; x := (y+3), {x → 4}
⇒ x := (y+3), {x → 4}
⇒ skip, {x → 3}'
}
