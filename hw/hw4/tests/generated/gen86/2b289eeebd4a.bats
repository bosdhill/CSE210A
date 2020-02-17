load ../../harness

@test "2b289eeebd4a" {
  check 'x     := z   *1  ' '⇒ skip, {x → 0}'
}
