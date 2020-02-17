load ../../harness

@test "71e708065ce4" {
  check 'y := 0    -     x  ' '⇒ skip, {y → 0}'
}
