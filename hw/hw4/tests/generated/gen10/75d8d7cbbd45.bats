load ../../harness

@test "75d8d7cbbd45" {
  check 'x :=  y   -    y' '⇒ skip, {x → 0}'
}
