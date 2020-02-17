load ../../harness

@test "31a630f4ceb0" {
  check 'x:=     y  ;skip    ' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
