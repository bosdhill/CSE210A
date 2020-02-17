load ../../harness

@test "4b92d83a09e1" {
  check 'skip ;y:= -3    +    g     ' '⇒ y := (-3+g), {}
⇒ skip, {y → -3}'
}
