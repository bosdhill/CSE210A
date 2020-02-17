load ../../harness

@test "208279497bfb" {
  check 'x := Q  *1   ' '⇒ skip, {x → 0}'
}
