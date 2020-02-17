load ../../harness

@test "a1de63e517af" {
  check 'x := -2  *   y' '⇒ skip, {x → 0}'
}
