load ../../harness

@test "645b7c31e4e1" {
  check 'x     :=     x   ; skip ' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
