load ../../harness

@test "4b5c6296668e" {
  check 'x := x +  x' '⇒ skip, {x → 0}'
}
