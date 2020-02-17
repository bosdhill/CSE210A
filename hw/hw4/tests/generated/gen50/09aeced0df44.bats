load ../../harness

@test "09aeced0df44" {
  check 'x:=    y  ;skip    ' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
