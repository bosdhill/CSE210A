load ../../harness

@test "8ab4b6f19d8d" {
  check 'x:=x  *     y   ;  skip    ' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
