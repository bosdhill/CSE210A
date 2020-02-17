load ../../harness

@test "61ed37e73dcf" {
  check 'x:=    y     +   y ; skip  ' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
