load ../../harness

@test "35411599ea80" {
  check 'x    := 2   +-2   ; y   :=   qb  *  3  ' '⇒ skip; y := (qb*3), {x → 0}
⇒ y := (qb*3), {x → 0}
⇒ skip, {x → 0, y → 0}'
}
