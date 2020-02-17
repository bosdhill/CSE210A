load ../../harness

@test "22c5c0e56cbd" {
  check 'if true     then   x   := 1     +   x  else 

 skip     ' '⇒ x := (1+x), {}
⇒ skip, {x → 1}'
}
