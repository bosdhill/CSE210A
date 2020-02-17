load ../../harness

@test "584980895ebf" {
  check 'while true   ∧    x - y<y  +   y   do skip    ' '⇒ skip, {}'
}
