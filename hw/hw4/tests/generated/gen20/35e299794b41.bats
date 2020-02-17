load ../../harness

@test "35e299794b41" {
  check 'skip;  x :=-3   - x   ' '⇒ x := (-3-x), {}
⇒ skip, {x → -3}'
}
