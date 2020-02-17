load ../../harness

@test "45beeba198a7" {
  check 'x :=  y     *     SG   ;  
  x :=     y   *y  ' '⇒ skip; x := (y*y), {x → 0}
⇒ x := (y*y), {x → 0}
⇒ skip, {x → 0}'
}
