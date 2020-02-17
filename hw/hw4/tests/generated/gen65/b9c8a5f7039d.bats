load ../../harness

@test "b9c8a5f7039d" {
  check 'y:=y  *-3   ;  x    :=   y     - y    ' '⇒ skip; x := (y-y), {y → 0}
⇒ x := (y-y), {y → 0}
⇒ skip, {x → 0, y → 0}'
}
