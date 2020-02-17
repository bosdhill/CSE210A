load ../../harness

@test "844c0f776bbc" {
  check 'x:=    1   -   -2' '⇒ skip, {x → 3}'
}
