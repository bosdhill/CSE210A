load ../../harness

@test "5fbaa318d255" {
  check 'x :=    -3  ;z  :=0     ' '⇒ skip; z := 0, {x → -3}
⇒ z := 0, {x → -3}
⇒ skip, {x → -3, z → 0}'
}
