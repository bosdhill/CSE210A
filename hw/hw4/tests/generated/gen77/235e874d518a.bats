load ../../harness

@test "235e874d518a" {
  check 'x     := x  +    -3    ;skip  ' '⇒ skip; skip, {x → -3}
⇒ skip, {x → -3}'
}
