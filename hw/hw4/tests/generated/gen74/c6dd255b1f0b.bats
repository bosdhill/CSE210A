load ../../harness

@test "c6dd255b1f0b" {
  check 'x:=    1 * z    ;skip     ' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
