load ../../harness

@test "a8f475be51e5" {
  check 'if false      then 
skip      else  y    :=     gn     * x   ' '⇒ y := (gn*x), {}
⇒ skip, {y → 0}'
}
