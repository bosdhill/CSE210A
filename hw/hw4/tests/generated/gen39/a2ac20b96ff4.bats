load ../../harness

@test "a2ac20b96ff4" {
  check 'g9    :=    y  *uf ;z  :=     3    +    -3' '⇒ skip; z := (3+-3), {g9 → 0}
⇒ z := (3+-3), {g9 → 0}
⇒ skip, {g9 → 0, z → 0}'
}
