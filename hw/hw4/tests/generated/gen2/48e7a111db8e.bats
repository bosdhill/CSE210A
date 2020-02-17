load ../../harness

@test "48e7a111db8e" {
  check 'x  :=-2*  y;x    :=   z     *0  ' '⇒ skip; x := (z*0), {x → 0}
⇒ x := (z*0), {x → 0}
⇒ skip, {x → 0}'
}
