load ../../harness

@test "560f73f3e252" {
  check 'skip    ;x     := z   --4    ' '⇒ x := (z--4), {}
⇒ skip, {x → 4}'
}
