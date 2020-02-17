load ../../harness

@test "9ffa0f8a48de" {
  check 'skip;x:= z* S ' '⇒ x := (z*S), {}
⇒ skip, {x → 0}'
}
