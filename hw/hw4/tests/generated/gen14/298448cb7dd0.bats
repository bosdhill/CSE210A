load ../../harness

@test "298448cb7dd0" {
  check 'y     :=-2--1     ;  x  :=   x    +   TO     ' '⇒ skip; x := (x+TO), {y → -1}
⇒ x := (x+TO), {y → -1}
⇒ skip, {x → 0, y → -1}'
}
