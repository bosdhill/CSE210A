load ../../harness

@test "5e47dbf18c74" {
  check 'skip   ; x  :=    -1 -  2  ' '⇒ x := (-1-2), {}
⇒ skip, {x → -3}'
}
