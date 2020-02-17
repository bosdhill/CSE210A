load ../../harness

@test "2f827c6a5eb9" {
  check 'if (¬(y    -y    <  4  -  -3))     then y     :=1 * y  else x :=     2 * 1 ' '⇒ x := (2*1), {}
⇒ skip, {x → 2}'
}
