load ../../harness

@test "86e7ddef130b" {
  check 'x:=    1   *  -4  ;    skip  ' '⇒ skip; skip, {x → -4}
⇒ skip, {x → -4}'
}
