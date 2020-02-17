load ../../harness

@test "fdc5b4efc4f1" {
  check 'skip  ; x    :=-3' '⇒ x := -3, {}
⇒ skip, {x → -3}'
}
