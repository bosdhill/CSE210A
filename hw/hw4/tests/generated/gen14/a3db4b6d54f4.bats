load ../../harness

@test "a3db4b6d54f4" {
  check 'x:=    y - 1   ;skip   ' '⇒ skip; skip, {x → -1}
⇒ skip, {x → -1}'
}
