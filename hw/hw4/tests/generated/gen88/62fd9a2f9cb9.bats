load ../../harness

@test "62fd9a2f9cb9" {
  check 'T8 :=z *   x  ;z:=y   +  -3' '⇒ skip; z := (y+-3), {T8 → 0}
⇒ z := (y+-3), {T8 → 0}
⇒ skip, {T8 → 0, z → -3}'
}
