load ../../harness

@test "9b1772e8dacc" {
  check 'skip ;x:= 4+     -4' '⇒ x := (4+-4), {}
⇒ skip, {x → 0}'
}
