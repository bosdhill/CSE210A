load ../../harness

@test "fdd0636178cc" {
  check 'x :=x   -  x ' '⇒ skip, {x → 0}'
}
