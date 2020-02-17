load ../../harness

@test "16af256c2964" {
  check 'x   := y + 1' '⇒ skip, {x → 1}'
}
