load ../../harness

@test "47fae38ca7cb" {
  check 'skip   ;    x:= z* -4 ' '⇒ x := (z*-4), {}
⇒ skip, {x → 0}'
}
