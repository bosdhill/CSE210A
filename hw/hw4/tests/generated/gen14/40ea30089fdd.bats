load ../../harness

@test "40ea30089fdd" {
  check 'x    :=    y+   x   ;skip   ' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
