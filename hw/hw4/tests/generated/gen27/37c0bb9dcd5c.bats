load ../../harness

@test "37c0bb9dcd5c" {
  check 'x    :=  z ;skip' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
