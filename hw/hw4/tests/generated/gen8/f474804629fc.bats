load ../../harness

@test "f474804629fc" {
  check 'x   :=    z    -   y     ' '⇒ skip, {x → 0}'
}
