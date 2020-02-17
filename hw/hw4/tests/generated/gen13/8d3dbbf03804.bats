load ../../harness

@test "8d3dbbf03804" {
  check 'y :=    -2-     y    ' '⇒ skip, {y → -2}'
}
