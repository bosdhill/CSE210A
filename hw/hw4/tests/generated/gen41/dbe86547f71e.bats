load ../../harness

@test "dbe86547f71e" {
  check 'x     :=     1     - 4  ' '⇒ skip, {x → -3}'
}
