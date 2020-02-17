load ../../harness

@test "3193fe4e1f5c" {
  check 'x:=     P6+ x  ;
y    :=     x    - 2  ' '⇒ skip; y := (x-2), {x → 0}
⇒ y := (x-2), {x → 0}
⇒ skip, {x → 0, y → -2}'
}
