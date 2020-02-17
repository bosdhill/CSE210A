load ../../harness

@test "1433d092ccb9" {
  check 'skip ;x:= y  +     x    ' '⇒ x := (y+x), {}
⇒ skip, {x → 0}'
}
