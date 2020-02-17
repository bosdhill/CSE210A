load ../../harness

@test "3121efb7adf4" {
  check 'skip  ; w    :=   -4   -  4  ' '⇒ w := (-4-4), {}
⇒ skip, {w → -8}'
}
