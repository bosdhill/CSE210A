load ../../harness

@test "fdaabe09fb7b" {
  check 'x    :=   fP     *     x; Hf     :=     x    *   y    ' '⇒ skip; Hf := (x*y), {x → 0}
⇒ Hf := (x*y), {x → 0}
⇒ skip, {Hf → 0, x → 0}'
}
