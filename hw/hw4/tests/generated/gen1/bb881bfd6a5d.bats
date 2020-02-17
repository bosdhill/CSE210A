load ../../harness

@test "bb881bfd6a5d" {
  check 'q :=  x  -1   ; x   :=   H   + -4    ' '⇒ skip; x := (H+-4), {q → -1}
⇒ x := (H+-4), {q → -1}
⇒ skip, {q → -1, x → -4}'
}
