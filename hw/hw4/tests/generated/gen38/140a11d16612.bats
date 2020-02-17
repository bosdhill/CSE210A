load ../../harness

@test "140a11d16612" {
  check 'if (y  +-1   <y     -    -2     ∧   false)     then skip     else   y  :=3     *   -2' '⇒ y := (3*-2), {}
⇒ skip, {y → -6}'
}
