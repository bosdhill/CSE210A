load ../../harness

@test "e2d551d248fb" {
  check 'x  := 2   *z  ; skip   ' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
