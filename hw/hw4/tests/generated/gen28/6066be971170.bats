load ../../harness

@test "6066be971170" {
  check 'x :=  z *    -2   ;x :=   z  -  2' '⇒ skip; x := (z-2), {x → 0}
⇒ x := (z-2), {x → 0}
⇒ skip, {x → -2}'
}
