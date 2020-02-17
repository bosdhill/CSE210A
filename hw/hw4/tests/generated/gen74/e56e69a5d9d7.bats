load ../../harness

@test "e56e69a5d9d7" {
  check 'skip     ; 

y := 4 * x  ' '⇒ y := (4*x), {}
⇒ skip, {y → 0}'
}
