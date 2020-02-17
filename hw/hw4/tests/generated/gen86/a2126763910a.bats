load ../../harness

@test "a2126763910a" {
  check 'x  :=   x    *  z  ' '⇒ skip, {x → 0}'
}
