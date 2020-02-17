load ../../harness

@test "2099ebfa4c58" {
  check 'x:= 2   *   z   ;     skip   ' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
