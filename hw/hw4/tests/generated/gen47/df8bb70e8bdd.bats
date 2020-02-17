load ../../harness

@test "df8bb70e8bdd" {
  check 'x   :=x*     y;   
y  :=   y    -    1  ' '⇒ skip; y := (y-1), {x → 0}
⇒ y := (y-1), {x → 0}
⇒ skip, {x → 0, y → -1}'
}
