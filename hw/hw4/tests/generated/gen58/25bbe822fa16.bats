load ../../harness

@test "25bbe822fa16" {
  check 'x   :=   1 -    x     ; 

y     :=    2 *     3   ' '⇒ skip; y := (2*3), {x → 1}
⇒ y := (2*3), {x → 1}
⇒ skip, {x → 1, y → 6}'
}
