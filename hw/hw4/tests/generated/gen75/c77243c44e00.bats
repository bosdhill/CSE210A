load ../../harness

@test "c77243c44e00" {
  check 'x    := -1   *     y  ;    skip  ' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
