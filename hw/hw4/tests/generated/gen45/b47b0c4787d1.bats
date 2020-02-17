load ../../harness

@test "b47b0c4787d1" {
  check 'x    :=   2     ;s  :=    x * x  ' '⇒ skip; s := (x*x), {x → 2}
⇒ s := (x*x), {x → 2}
⇒ skip, {s → 4, x → 2}'
}
