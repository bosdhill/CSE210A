load ../../harness

@test "e115857a2279" {
  check 'x :=     -2*    z     ;  z:= z +  z     ' '⇒ skip; z := (z+z), {x → 0}
⇒ z := (z+z), {x → 0}
⇒ skip, {x → 0, z → 0}'
}
