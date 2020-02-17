load ../../harness

@test "9bdf2ae7d080" {
  check 'skip    ;y :=x*     x   ' '⇒ y := (x*x), {}
⇒ skip, {y → 0}'
}
