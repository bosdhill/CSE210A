load ../../harness

@test "f92030958242" {
  check 'x     :=   -3*  x' '⇒ skip, {x → 0}'
}
