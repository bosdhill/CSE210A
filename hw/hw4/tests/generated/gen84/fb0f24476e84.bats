load ../../harness

@test "fb0f24476e84" {
  check 'while false∧    true  do  skip  ' '⇒ skip, {}'
}
