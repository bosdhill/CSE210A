load ../../harness

@test "10722b350622" {
  check 'skip  ; x   :=     z  -     4  ' '⇒ x := (z-4), {}
⇒ skip, {x → -4}'
}
