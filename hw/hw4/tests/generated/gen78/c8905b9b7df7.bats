load ../../harness

@test "c8905b9b7df7" {
  check 'x :=    x  *     -1   ; y   := -2  +    0   ' '⇒ skip; y := (-2+0), {x → 0}
⇒ y := (-2+0), {x → 0}
⇒ skip, {x → 0, y → -2}'
}
