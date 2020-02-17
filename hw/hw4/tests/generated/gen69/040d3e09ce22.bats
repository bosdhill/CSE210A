load ../../harness

@test "040d3e09ce22" {
  check 'x   :=   W- 1   ; x :=     x - -1   ' '⇒ skip; x := (x--1), {x → -1}
⇒ x := (x--1), {x → -1}
⇒ skip, {x → 0}'
}
