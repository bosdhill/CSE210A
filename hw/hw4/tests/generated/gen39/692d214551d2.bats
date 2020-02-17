load ../../harness

@test "692d214551d2" {
  check 'x :=y  ; y:=  q    +    -4 ' '⇒ skip; y := (q+-4), {x → 0}
⇒ y := (q+-4), {x → 0}
⇒ skip, {x → 0, y → -4}'
}
