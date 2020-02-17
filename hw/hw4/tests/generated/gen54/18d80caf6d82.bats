load ../../harness

@test "18d80caf6d82" {
  check 'x  :=z -    y   ;
y    :=   0    -   y  ' '⇒ skip; y := (0-y), {x → 0}
⇒ y := (0-y), {x → 0}
⇒ skip, {x → 0, y → 0}'
}
