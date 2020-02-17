load ../../harness

@test "b023dc2782a0" {
  check 'x     :=   0*y    ' '⇒ skip, {x → 0}'
}
