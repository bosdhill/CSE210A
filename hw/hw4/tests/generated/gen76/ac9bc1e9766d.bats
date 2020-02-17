load ../../harness

@test "ac9bc1e9766d" {
  check 'if false   then      skip    else y    :=x  *x    ' '⇒ y := (x*x), {}
⇒ skip, {y → 0}'
}
