load ../../harness

@test "6df131382514" {
  check 'x :=     -1    +   y    ; TP    :=     k--1   ' '⇒ skip; TP := (k--1), {x → -1}
⇒ TP := (k--1), {x → -1}
⇒ skip, {TP → 1, x → -1}'
}
