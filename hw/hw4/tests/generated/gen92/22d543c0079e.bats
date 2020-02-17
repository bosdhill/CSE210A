load ../../harness

@test "22d543c0079e" {
  check 'skip   ;x  :=   n *     z ' '⇒ x := (n*z), {}
⇒ skip, {x → 0}'
}
