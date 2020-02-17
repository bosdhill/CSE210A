load ../../harness

@test "fb6cc7b7a598" {
  check 'x :=    2  +  1 ;Qm :=z   * x ' '⇒ skip; Qm := (z*x), {x → 3}
⇒ Qm := (z*x), {x → 3}
⇒ skip, {Qm → 0, x → 3}'
}
