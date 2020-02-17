load ../../harness

@test "b760001bdec1" {
  check 'skip; y   := -3     -z    ' '⇒ y := (-3-z), {}
⇒ skip, {y → -3}'
}
