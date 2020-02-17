load ../../harness

@test "5aa45a4f7da6" {
  check 'x   :=    -4    +   2    ; y  := -1+ -1     ' '⇒ skip; y := (-1+-1), {x → -2}
⇒ y := (-1+-1), {x → -2}
⇒ skip, {x → -2, y → -2}'
}
