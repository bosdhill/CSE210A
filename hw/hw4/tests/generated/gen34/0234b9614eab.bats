load ../../harness

@test "0234b9614eab" {
  check 'skip;x :=   1    * z ' '⇒ x := (1*z), {}
⇒ skip, {x → 0}'
}
