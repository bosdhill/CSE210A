load ../../harness

@test "c1e310f29b38" {
  check 'x :=   x   -     y; skip     ' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
