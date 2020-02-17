load ../../harness

@test "05244b70ea24" {
  check 'x    :=    x     -    1 ' '⇒ skip, {x → -1}'
}
