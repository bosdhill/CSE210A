load ../../harness

@test "96084ed107e2" {
  check 'v :=   x   -     -4   ;x    :=    -1   -     k ' '⇒ skip; x := (-1-k), {v → 4}
⇒ x := (-1-k), {v → 4}
⇒ skip, {v → 4, x → -1}'
}
