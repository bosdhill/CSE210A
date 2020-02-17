load ../../harness

@test "35145eb46343" {
  check 'x  := A5  + y    ; skip     ' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
