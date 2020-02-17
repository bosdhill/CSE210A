load ../../harness

@test "256e518d590d" {
  check 'x     :=    3*4 ; y :=     3    +  0   ' '⇒ skip; y := (3+0), {x → 12}
⇒ y := (3+0), {x → 12}
⇒ skip, {x → 12, y → 3}'
}
