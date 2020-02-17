load ../../harness

@test "d51097f694c6" {
  check 'skip    ;   x  := z     -   2 ' '⇒ x := (z-2), {}
⇒ skip, {x → -2}'
}
