load ../../harness

@test "e4212729a685" {
  check 'x  :=  x' '⇒ skip, {x → 0}'
}
