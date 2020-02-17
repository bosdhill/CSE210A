load ../../harness

@test "452efb6395b3" {
  check 'x:=    -2  ;skip     ' '⇒ skip; skip, {x → -2}
⇒ skip, {x → -2}'
}
