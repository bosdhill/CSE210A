load ../../harness

@test "66b00bbc8858" {
  check 'x     :=2    ; y     :=     -3     -    -1    ' '⇒ skip; y := (-3--1), {x → 2}
⇒ y := (-3--1), {x → 2}
⇒ skip, {x → 2, y → -2}'
}
