load ../../harness

@test "8a14f44e4d5d" {
  check 'skip ; y    :=x *z   ' '⇒ y := (x*z), {}
⇒ skip, {y → 0}'
}
