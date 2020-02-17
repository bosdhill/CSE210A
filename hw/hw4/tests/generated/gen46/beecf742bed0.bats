load ../../harness

@test "beecf742bed0" {
  check 'eT :=     2  +-2 ; x     :=   -2    * y    ' '⇒ skip; x := (-2*y), {eT → 0}
⇒ x := (-2*y), {eT → 0}
⇒ skip, {eT → 0, x → 0}'
}
