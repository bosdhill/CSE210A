load ../../harness

@test "bde0c7e0fdf7" {
  check 'x     :=    x * 4    ' '⇒ skip, {x → 0}'
}
