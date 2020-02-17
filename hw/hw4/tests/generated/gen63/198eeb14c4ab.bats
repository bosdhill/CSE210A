load ../../harness

@test "198eeb14c4ab" {
  check 'x :=1     -  2' '⇒ skip, {x → -1}'
}
