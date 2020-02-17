load ../../harness

@test "c4214d436c18" {
  check 'x:=     2    *   e  ;skip' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
