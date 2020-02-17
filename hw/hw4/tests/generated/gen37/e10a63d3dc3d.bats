load ../../harness

@test "e10a63d3dc3d" {
  check 'x    :=    z     +    z     ' '⇒ skip, {x → 0}'
}
