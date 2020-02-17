load ../../harness

@test "32c9e952a079" {
  check 'u0    :=x     +    y    ;
  z  :=    z *  -4  ' '⇒ skip; z := (z*-4), {u0 → 0}
⇒ z := (z*-4), {u0 → 0}
⇒ skip, {u0 → 0, z → 0}'
}
