load ../../harness

@test "f7d59b61c0d3" {
  check 'x   :=   -4 -    y    ;n  :=     BD*     x  ' '⇒ skip; n := (BD*x), {x → -4}
⇒ n := (BD*x), {x → -4}
⇒ skip, {n → 0, x → -4}'
}
