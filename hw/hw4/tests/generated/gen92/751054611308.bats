load ../../harness

@test "751054611308" {
  check 'y  := y  +  y     ' '⇒ skip, {y → 0}'
}
