load ../../harness

@test "679f7e5ffbab" {
  check 'skip ;x   :=     -3   ' '⇒ x := -3, {}
⇒ skip, {x → -3}'
}
