load ../../harness

@test "7ddf0c8d09dc" {
  check 'D1:=    0     +    y  ;x     :=   -4   -   x  ' '⇒ skip; x := (-4-x), {D1 → 0}
⇒ x := (-4-x), {D1 → 0}
⇒ skip, {D1 → 0, x → -4}'
}
