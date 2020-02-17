load ../../harness

@test "525314d19097" {
  check 'y    := 3    * x  ' '⇒ skip, {y → 0}'
}
