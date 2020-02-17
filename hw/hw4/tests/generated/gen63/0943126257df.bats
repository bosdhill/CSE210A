load ../../harness

@test "0943126257df" {
  check 'x   := z+ x   ' '⇒ skip, {x → 0}'
}
