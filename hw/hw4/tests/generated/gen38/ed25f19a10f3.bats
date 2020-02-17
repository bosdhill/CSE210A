load ../../harness

@test "ed25f19a10f3" {
  check 'skip   ;x  := 4  +     E ' '⇒ x := (4+E), {}
⇒ skip, {x → 4}'
}
