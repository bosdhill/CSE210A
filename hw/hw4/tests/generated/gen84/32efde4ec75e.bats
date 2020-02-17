load ../../harness

@test "32efde4ec75e" {
  check 'y:= x+  z     ' '⇒ skip, {y → 0}'
}
