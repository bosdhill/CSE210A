load ../../harness

@test "6cb2f09aa796" {
  check 'x  :=     x9 -    2 ;z  :=    x     ' '⇒ skip; z := x, {x → -2}
⇒ z := x, {x → -2}
⇒ skip, {x → -2, z → -2}'
}
