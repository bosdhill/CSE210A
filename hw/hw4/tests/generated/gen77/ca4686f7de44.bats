load ../../harness

@test "ca4686f7de44" {
  check 'skip ;x     :=   0  +    -4   ' '⇒ x := (0+-4), {}
⇒ skip, {x → -4}'
}
