load ../../harness

@test "7d2a7e642648" {
  check 'x    :=0+     3  ; 
y:=   E   +-3 ' '⇒ skip; y := (E+-3), {x → 3}
⇒ y := (E+-3), {x → 3}
⇒ skip, {x → 3, y → -3}'
}
