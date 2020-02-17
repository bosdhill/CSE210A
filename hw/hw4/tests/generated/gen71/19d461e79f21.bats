load ../../harness

@test "19d461e79f21" {
  check 'x :=  rZ     +    x    ' '⇒ skip, {x → 0}'
}
