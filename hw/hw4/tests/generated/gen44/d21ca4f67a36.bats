load ../../harness

@test "d21ca4f67a36" {
  check 'u1:=     t  *   y  ;z   := 3     -  y     ' '⇒ skip; z := (3-y), {u1 → 0}
⇒ z := (3-y), {u1 → 0}
⇒ skip, {u1 → 0, z → 3}'
}
