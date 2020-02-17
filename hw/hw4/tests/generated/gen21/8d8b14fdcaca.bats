load ../../harness

@test "8d8b14fdcaca" {
  check 'x   := -1 *   y ; skip   ' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
