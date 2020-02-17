load ../../harness

@test "90c3fc3fdffe" {
  check 'x  :=   z  *    zb  ;    b :=  y     * x    ' '⇒ skip; b := (y*x), {x → 0}
⇒ b := (y*x), {x → 0}
⇒ skip, {b → 0, x → 0}'
}
