load ../../harness

@test "67fda94ac4d7" {
  check 'x  :=   3    +  -3  ;X2  :=2+-1' '⇒ skip; X2 := (2+-1), {x → 0}
⇒ X2 := (2+-1), {x → 0}
⇒ skip, {X2 → 1, x → 0}'
}
