load ../../harness

@test "3c546a661f5c" {
  check 'skip     ;x  :=  -3* x ' '⇒ x := (-3*x), {}
⇒ skip, {x → 0}'
}
