load ../../harness

@test "b9f038320dc9" {
  check 'y   := y   -  x ' '⇒ skip, {y → 0}'
}
